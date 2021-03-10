package main

import (
	"errors"
	"fc"
	"flag"
	"log"
	"os"
	"strings"
)

// file type patterns
var (
	audioFilePatterns  = []string{"*.ape", "*.flac", "*.mp3", "*.ogg", "*.opus", "*.m4a"}
	backupFilePatterns = []string{"*.bak", "*.old"}
	bookFilePatterns   = []string{"*.pdf", "*.azw3", "*.epub", "*.mobi"}
	codeFilePatterns   = []string{"*.asm", "*.cpp", "*.[hc]", "*.el", "*.go", "*.rb", "*.pl", "*.py", "*.sh"}
	confFilePatterns   = []string{"GNUmakefile", "Makefile", "makefile", "scons", "sconstruct", "*.conf", "*.ini", "*.json", "*.ld", "*.mk", "*.rc", "*.yml"}
	docFilePatterns    = []string{"*.md", "*.org", "*.tex", "*.txt"}
	htmlFilePatterns   = []string{"*.htm", "*.html", "*.xhtml", "*.xml"}
	imageFilePatterns  = []string{"*.bmp", "*.gif", "*.jpeg", "*.jpg", "*.tiff", "*.png"}
	videoFilePatterns  = []string{"*.mp4", "*.mkv", "*.avi", "*.webm", "*.mov", "*.wmv"}
)

// flag var
var (
	pathArg        = fc.Var("a", "include path")
	patternArg     = fc.Var("p", "name pattern")
	excludePathArg = fc.Var("x", "exclude path")

	longArg     = flag.Bool("l", false, "long format")
	debugArg    = flag.Bool("debug", false, "debug mode")
	rootArg     = flag.Bool("r", false, "auto jump to project root")
	rootPathArg = flag.String("rp", "", "set start dir to project root")
	audioArg    = flag.Bool("audio", false, "find audio files")
	bakArg      = flag.Bool("bak", false, "find backup files")
	binArg      = flag.Bool("bin", false, "find binary files")
	bookArg     = flag.Bool("book", false, "find book files")
	codeArg     = flag.Bool("code", false, "find source code files")
	confArg     = flag.Bool("conf", false, "find config files")
	docArg      = flag.Bool("doc", false, "find document files")
	htmlArg     = flag.Bool("html", false, "find html files")
	imageArg    = flag.Bool("img", false, "find image files")
	mediaArg    = flag.Bool("media", false, "find media files")
	videoArg    = flag.Bool("video", false, "find video files")
	noColorArg  = flag.Bool("nocolor", false, "disable ansi color")
	mtimeArg    = flag.String("mtime", "", "find files that was last modified n*24 hours ago")
	newerArg    = flag.String("newer", "", "find files newer than this")
	sizeArg     = flag.String("size", "", "find files with size")
	deleteArg   = flag.Bool("delete", false, "delete files")
	selectArg   = flag.Bool("s", false, "select file")
	emacsArg    = flag.Bool("ec", false, "edit in emacs")
	pagerArg    = flag.Bool("P", false, "output to pager")
)

func ChProjRoot() error {
	current, _ := os.Getwd()

	for {
		wd, err := os.Getwd()

		if os.IsNotExist(err) {
			os.Chdir(current)
			return errors.New("Project not found")
		}

		if fc.FileExists(".TOP") || fc.FileExists("TOP") {
			return nil
		}

		if wd == "/" {
			os.Chdir(current)
			return errors.New("Project not found")
		}

		os.Chdir("..")
	}
}

func addType(cond bool, totalPatterns *[]string, typePatterns []string) {
	if cond {
		*totalPatterns = append(*totalPatterns, typePatterns...)
	}
}

func fuzzyName(pattern string) []string {
	if pattern[0] == '/' {
		return []string{"-iregex", ".+/" + pattern[1:]}
	}

	if strings.Contains(pattern, "*") {
		return []string{"-iname", pattern}
	}

	return []string{"-iname", "*" + pattern + "*"}
}

func _init() {
	fc.Init()

	flag.Parse()

	if *mediaArg {
		*audioArg = true
		*imageArg = true
		*videoArg = true
	}
}

func main() {
	_init()

	if *rootArg {
		if *rootPathArg == "" {
			if err := ChProjRoot(); err != nil {
				log.Print(err)
				return
			}
		} else {
			os.Chdir(*rootPathArg)
		}
	}

	*excludePathArg = append(*excludePathArg, "*/.[^.]*", "*/depend", "*/__pycache__")

	namePatterns := []string{}

	if *debugArg {
		log.Printf("otherthings = %v", flag.Args())
	}

	for _, n := range flag.Args() {
		if fc.DirExists(n) {
			*pathArg = append(*pathArg, n)
		} else {
			namePatterns = append(namePatterns, n)
		}
	}

	namePatterns = append(namePatterns, *patternArg...)

	addType(*audioArg, &namePatterns, audioFilePatterns)
	addType(*bakArg, &namePatterns, backupFilePatterns)
	addType(*bookArg, &namePatterns, bookFilePatterns)
	addType(*codeArg, &namePatterns, codeFilePatterns)
	addType(*confArg, &namePatterns, confFilePatterns)
	addType(*docArg, &namePatterns, docFilePatterns)
	addType(*htmlArg, &namePatterns, htmlFilePatterns)
	addType(*imageArg, &namePatterns, imageFilePatterns)
	addType(*videoArg, &namePatterns, videoFilePatterns)

	if *debugArg {
		log.Printf("namePatterns = %v\n", namePatterns)
	}

	namePatternParam := []string{}
	if len(namePatterns) > 0 {
		for _, pattern := range namePatterns {
			namePatternParam = append(namePatternParam, "-o")
			namePatternParam = append(namePatternParam, fuzzyName(pattern)...)
		}
	} else {
		namePatternParam = append(namePatternParam, "-o", "-name", "*")
	}

	namePatternParam = append([]string{"("}, namePatternParam[1:]...)
	namePatternParam = append(namePatternParam, ")")

	if *longArg {
		namePatternParam = append(namePatternParam, "-printf", "\x1b[31m%M\x1b[0m \x1b[36m%u %g\x1b[0m \x1b[34m%7kK\x1b[0m \x1b[35m[%TY-%Tm-%Td]\x1b[0m \x1b[32m%h/\x1b[0m\x1b[33m%f\x1b[0m\n")
	} else if *noColorArg {
		namePatternParam = append(namePatternParam, "-printf", "%h/%f\n")
	} else {
		namePatternParam = append(namePatternParam, "-printf", "\x1b[32m%h/\x1b[0m\x1b[33m%f\x1b[0m\n")
	}

	if *debugArg {
		log.Printf("excludePath = %v", excludePathArg)
	}

	excludePathParam := []string{}
	for _, pattern := range *excludePathArg {
		excludePathParam = append(excludePathParam, "-o", "-path", pattern)
	}

	excludePathParam = append([]string{"("}, excludePathParam[1:]...)
	excludePathParam = append(excludePathParam, ")", "-prune")

	var ignoreParam []string
	if *binArg {
		ignoreParam = []string{"-o", "-regex", ".*\\.o\\|.*~\\|.*\\.d\\|.*#", "-prune"}
	} else {
		ignoreParam = []string{"-o", "-regex", ".*\\.o\\|.*~\\|.*\\.d\\|.*#\\|.*\\.a", "-prune"}
	}

	findParams := []string{os.Getenv("FIND")}

	if findParams[0] == "" {
		findParams[0] = "find"
	}

	findParams = append(findParams, *pathArg...)
	findParams = append(findParams, excludePathParam...)
	findParams = append(findParams, "-o", "-type", "d")
	findParams = append(findParams, ignoreParam...)
	findParams = append(findParams, "-o", "(")

	if len(*sizeArg) > 0 {
		findParams = append(findParams, "-size", *sizeArg)
	}
	if len(*newerArg) > 0 {
		findParams = append(findParams, "-newer", *newerArg)
	}
	if len(*mtimeArg) > 0 {
		findParams = append(findParams, "-mtime", *mtimeArg)
	}
	findParams = append(findParams, namePatternParam...)
	findParams = append(findParams, ")")

	if *debugArg {
		log.Printf("findParams = %v", findParams)
	}

	fzfParam := []string{"fzf", "-m", "-e", "--ansi"}

	var cmds [][]string

	if *deleteArg {
		cmds = [][]string{
			findParams,
			{"xargs", "rm", "-f"},
		}
	} else if *selectArg {
		cmds = [][]string{
			findParams,
			fzfParam,
		}
	} else if *emacsArg {
		cmds = [][]string{
			findParams,
			fzfParam,
			{"xargs", "emacsclient", "-n"},
		}
	} else {
		cmds = [][]string{
			findParams,
		}
	}

	if *pagerArg {
		cmds = append(cmds, []string{"less", "-r"})
	}

	fc.Pipe(cmds)
}
