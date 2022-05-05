package main

import (
	"bufio"
	"fconfig/fc"
	"flag"
	"fmt"
	"io/ioutil"
	"log"
	"os"
	"path/filepath"
	"regexp"
	"strings"
	"sync"
	"time"
)

type searchResult struct {
	dir       string
	fileInfos []os.FileInfo
}

var (
	pool *fc.WorkerPool
	iowg sync.WaitGroup

	dirFilter   func(string, os.FileInfo) bool
	fileFilters []func(os.FileInfo) bool
)

// flag var
var (
	pathArg        = fc.Var("a", "include path")
	excludePathArg = fc.Var("x", "exclude path")

	profArg     = flag.Bool("prof", false, "profile")
	longArg     = flag.Bool("l", false, "long format")
	debugArg    = flag.Bool("debug", false, "debug mode")
	rootArg     = flag.Bool("r", false, "auto jump to project root")
	rootPathArg = flag.String("rp", "", "set start dir to project root")

	audioArg  = flag.Bool("audio", false, "find audio files")
	bakArg    = flag.Bool("bak", false, "find backup files")
	bookArg   = flag.Bool("book", false, "find book files")
	codeArg   = flag.Bool("code", false, "find source code files")
	confArg   = flag.Bool("conf", false, "find config files")
	docArg    = flag.Bool("doc", false, "find document files")
	htmlArg   = flag.Bool("html", false, "find html files")
	imageArg  = flag.Bool("img", false, "find image files")
	libArg    = flag.Bool("lib", false, "find lib files")
	lightArg  = flag.Bool("light", false, "light theme")
	mediaArg  = flag.Bool("media", false, "find media files")
	officeArg = flag.Bool("office", false, "find office files")
	pkgArg    = flag.Bool("pkg", false, "find package files")
	videoArg  = flag.Bool("video", false, "find video files")
	xmlArg    = flag.Bool("xml", false, "find xml files")

	colorArg   = flag.Bool("color", false, "enable ansi color")
	noColorArg = flag.Bool("nocolor", false, "disable ansi color")
	ncArg      = flag.Bool("nc", false, "disable ansi color")

	lowerTimeArg = flag.String("ltime", "", "find files that younger than this arg")
	upperTimeArg = flag.String("utime", "", "find files that older than this arg")

	lowerSizeArg = flag.String("lsize", "0", "find files bigger than this lower size")
	upperSizeArg = flag.String("usize", "0", "find files smaller than this upper size")

	verboseArg = flag.Bool("v", false, "verbose flag")
)

func filterFile(parent string, fileInfo os.FileInfo) bool {
	for _, x := range fileFilters {
		if !x(fileInfo) {
			return false
		}
	}

	return true
}

func processDir(dir string, resultChannel chan *searchResult) {
	subdirs := make([]string, 0, 32)

	for {
		subdirs = append(subdirs, doProcessDir(dir, resultChannel)...)

		if len(subdirs) == 0 {
			return
		}

		if len(subdirs) == 1 {
			dir = subdirs[0]
			subdirs = subdirs[:0]
		} else if pool.IsBusy() {
			dir = subdirs[len(subdirs)-1]
			subdirs = subdirs[:len(subdirs)-1]
		} else {
			addDir(subdirs[len(subdirs)-1], resultChannel)
			dir = subdirs[len(subdirs)-2]
			subdirs = subdirs[:len(subdirs)-2]
		}
	}
}

func doProcessDir(dir string, resultChannel chan *searchResult) []string {
	subdirs := make([]string, 0, 32)
	fileInfos := make([]os.FileInfo, 0, 256)

	files, err := ioutil.ReadDir(dir)
	if err != nil {
		if *verboseArg {
			println("open dir failed", dir)
		}

		return []string{}
	}

	for _, file := range files {
		switch {
		case file.IsDir():
			if dirFilter(dir, file) {
				if dir == "." {
					subdirs = append(subdirs, file.Name())
				} else {
					subdirs = append(subdirs, dir+"/"+file.Name())
				}
			}

		case file.Mode().IsRegular():
			if filterFile(dir, file) {
				fileInfos = append(fileInfos, file)
			}
		}
	}

	resultChannel <- &searchResult{dir, fileInfos}

	return subdirs
}

func getLongFormatStr() string {
	if *lightArg {
		return "\x1b[31m%v\x1b[0m \x1b[34m%s\x1b[0m \x1b[35m%s\x1b[0m\x1b[30m%s\x1b[0m\n"
	}

	return "\x1b[33m%v\x1b[0m \x1b[31m%s\x1b[0m \x1b[35m%s\x1b[0m\x1b[37m%s\x1b[0m\n"
}

func getShortFormatStr() string {
	if *lightArg {
		return "\x1b[31m%s\x1b[0m\x1b[30m%s\x1b[0m\n"
	}
	return "\x1b[33m%s\x1b[0m\x1b[37m%s\x1b[0m\n"
}

func printOut(b *bufio.Writer, dir *string, fi os.FileInfo) {
	ndir := ""

	if *dir != "." {
		ndir = *dir + "/"
	}

	switch {
	case *longArg && *noColorArg:
		fmt.Fprintf(
			b,
			"%v %s %s%s\n",
			fi.Mode(),
			fc.GenHumanSize(fi.Size()),
			ndir,
			fi.Name())

	case *longArg:
		fmt.Fprintf(
			b,
			getLongFormatStr(),
			fi.Mode(),
			fc.GenHumanSize(fi.Size()),
			ndir,
			fi.Name())

	case *noColorArg:
		fmt.Fprintf(b, "%s%s\n", ndir, fi.Name())

	default:
		fmt.Fprintf(b, getShortFormatStr(), ndir, fi.Name())
	}
}

func processResult(resultChannel chan *searchResult) {
	lines := 0
	iowg.Add(1)
	defer iowg.Done()

	b := bufio.NewWriter(os.Stdout)
	defer b.Flush()

	for rst := range resultChannel {
		if len(rst.dir) == 0 {
			return
		}

		for _, fi := range rst.fileInfos {
			lines++
			printOut(b, &rst.dir, fi)

			if lines > 256 {
				lines = 0
				b.Flush()
			}
		}
	}
}

func filterIgnoreFiles(fileInfo os.FileInfo) bool {
	name := fileInfo.Name()

	if strings.HasSuffix(name, "~") || strings.HasPrefix(name, ".") {
		return false
	}

	return true
}

func filterFileSize() func(os.FileInfo) bool {
	var (
		err            error
		sizeUpperLimit int64
		sizeLowerLimit int64
	)

	if sizeLowerLimit, err = fc.ParseHumanSize(*lowerSizeArg); err != nil {
		log.Fatal("parse lower limit size error")
	}

	if sizeUpperLimit, err = fc.ParseHumanSize(*upperSizeArg); err != nil {
		log.Fatal("parse upper limit size error")
	}

	return func(fileInfo os.FileInfo) bool {
		if sizeUpperLimit > 0 && fileInfo.Size() > sizeUpperLimit {
			return false
		}

		if sizeLowerLimit > 0 && fileInfo.Size() < sizeLowerLimit {
			return false
		}

		return true
	}
}

func filterFileTime() func(os.FileInfo) bool {
	var (
		err error

		timeUpperLimit time.Time
		timeLowerLimit time.Time
	)

	if timeLowerLimit, err = fc.ParseHumanTime(*lowerTimeArg); err != nil {
		log.Fatal("parse lower limit time error")
	}

	if timeUpperLimit, err = fc.ParseHumanTime(*upperTimeArg); err != nil {
		log.Fatal("parse upper limit time error")
	}

	return func(fi os.FileInfo) bool {
		if !timeUpperLimit.IsZero() && fi.ModTime().After(timeUpperLimit) {
			return false
		}

		if !timeLowerLimit.IsZero() && fi.ModTime().Before(timeLowerLimit) {
			return false
		}

		return true
	}
}

func filterDir() func(string, os.FileInfo) bool {
	var ignoreDirNameReg *regexp.Regexp
	ignoreDirNameMap := map[string]bool{
		"depend":      true,
		"__pycache__": true,
	}

	patterns := []string{}

	for _, x := range *excludePathArg {
		patterns = append(patterns, x)
	}

	if len(patterns) > 0 {
		ignoreDirNameReg = regexp.MustCompile(strings.Join(patterns, "|"))
	}

	return func(parent string, fi os.FileInfo) bool {
		name := fi.Name()

		if strings.HasPrefix(name, ".") {
			return false
		}

		if ignoreDirNameMap[name] {
			return false
		}

		if ignoreDirNameReg != nil && ignoreDirNameReg.MatchString(parent+"/"+name) {
			return false
		}

		return true
	}
}

func addFilename(m map[string]bool, filenames []string) {
	for _, x := range filenames {
		m[x] = true
	}
}

func addFileExt(m map[string]bool, exts []string) {
	for _, x := range exts {
		m[x] = true
	}
}

func filterType() func(os.FileInfo) bool {
	extMap := make(map[string]bool)
	typeFileNameMap := make(map[string]bool)

	if *audioArg {
		addFileExt(extMap, []string{".aiff", ".alac", ".ape", ".flac", ".mp3", ".ogg", ".opus", ".m4a", ".wav", ".wma"})
	}
	if *bakArg {
		addFileExt(extMap, []string{".bak", ".old"})
	}
	if *bookArg {
		addFileExt(extMap, []string{".azw3", ".epub", ".mobi", ".pdf"})
	}
	if *codeArg {
		addFileExt(extMap, []string{".awk", ".c", ".h", ".cpp", ".el", ".go", ".hs", ".lua", ".rb", ".rst", ".pl", ".py", ".sh", ".vim", ".y", ".zsh"})
	}
	if *confArg {
		addFilename(typeFileNameMap, []string{"CMakeLists.txt", "GNUmakefile", "makefile", "Makefile"})
		addFileExt(extMap, []string{".cmake", ".conf", ".int", ".json", ".ld", ".mk", ".rc", ".yml"})
	}
	if *docArg {
		addFileExt(extMap, []string{".md", ".org", ".tex", ".txt"})
	}
	if *imageArg {
		addFileExt(extMap, []string{".bmp", ".gif", ".jpeg", ".jpg", ".tiff", ".png", ".svg"})
	}
	if *libArg {
		addFileExt(extMap, []string{".a", ".so"})
	}
	if *officeArg {
		addFileExt(extMap, []string{".doc", ".docm", ".docx", ".dot", ".dotm", ".ods", ".odt", ".ppt", ".pptx", ".xls", ".xlsx", ".wps"})
	}
	if *pkgArg {
		addFileExt(extMap, []string{".7z", ".deb", ".rar", ".rpm", ".tar.bz2", ".tar.gz", ".zip"})
	}
	if *videoArg {
		addFileExt(extMap, []string{".avi", ".mkv", ".mov", ".mp4", ".webm", ".wmv"})
	}
	if *xmlArg {
		addFileExt(extMap, []string{".dtd", ".xml", ".xsl", ".xslt"})
	}

	if len(extMap) == 0 {
		extMap = nil
	}

	if len(typeFileNameMap) == 0 {
		typeFileNameMap = nil
	}

	return func(fi os.FileInfo) bool {
		name := fi.Name()
		ext := strings.ToLower(filepath.Ext(name))

		if typeFileNameMap != nil && typeFileNameMap[name] {
			return true
		}

		if extMap != nil && !extMap[ext] {
			return false
		}

		return true
	}
}

func produceUserFileNameRegexPattern() (string, bool) {
	patterns := []string{}

	for _, x := range flag.Args() {
		patterns = append(patterns, x)
	}

	if len(patterns) > 0 {
		return strings.Join(patterns, "|"), true
	}

	return "", false
}

func filterFileNamePattern() func(fi os.FileInfo) bool {
	var (
		userFileNameReg *regexp.Regexp
	)

	if pattern, ok := produceUserFileNameRegexPattern(); ok {
		userFileNameReg = regexp.MustCompile(pattern)
	}

	return func(fi os.FileInfo) bool {
		return userFileNameReg.MatchString(fi.Name())
	}
}

func addDir(dir string, resultChannel chan *searchResult) {
	ndir := dir

	pool.AddJob(
		func() {
			processDir(
				ndir,
				resultChannel,
			)
		})
}

func parseArg() {
	flag.Parse()

	if *mediaArg {
		*audioArg = true
		*imageArg = true
		*videoArg = true
	}

	if *ncArg {
		*noColorArg = true
	}
}

func setupEnv() {
	if *rootArg {
		if *rootPathArg == "" {
			if err := fc.ChdirProjRoot(); err != nil {
				log.Print(err)
				return
			}
		} else {
			os.Chdir(*rootPathArg)
		}
	}

	fileFilters = append(fileFilters, filterIgnoreFiles)

	if *lowerSizeArg != "0" || *upperSizeArg != "0" {
		fileFilters = append(fileFilters, filterFileSize())
	}

	if len(*lowerTimeArg) != 0 || len(*upperTimeArg) != 0 {
		fileFilters = append(fileFilters, filterFileTime())
	}

	fileFilters = append(fileFilters, filterType())

	if len(flag.Args()) != 0 {
		fileFilters = append(fileFilters, filterFileNamePattern())
	}

	dirFilter = filterDir()

	if *profArg {
		fc.StartProfile()
		defer fc.StopProfile()
	}

	if "dumb" == os.Getenv("TERM") {
		*noColorArg = true
	}

	if "true" == os.Getenv("FC_LIGHT_THEME") {
		*lightArg = true
	}
}

func run() {
	resultChannel := make(chan *searchResult, 16*1024)

	go processResult(resultChannel)

	pool = fc.NewWorkerPool(0, 4*1024)

	if len(*pathArg) == 0 {
		addDir(".", resultChannel)
	} else {
		for _, x := range *pathArg {
			addDir(x, resultChannel)
		}
	}

	pool.WaitPool()

	resultChannel <- &searchResult{"", nil}
	iowg.Wait()

	if *verboseArg {
		println(pool.Info())
	}
}

func main() {
	fc.Init()
	fc.DisableGC()

	parseArg()

	setupEnv()

	run()
}
