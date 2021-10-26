package main

import (
	"fconfig/fc"
	"flag"
	"fmt"
	"os"
	"os/exec"
	"regexp"
	"strings"
)

var (
	chinese_mode   = flag.Bool("c", false, "Set chinese mode")
	utf8_conv_flag = flag.Bool("u", false, "Convert to utf8")
	verbose_mode   = flag.Bool("v", false, "Set verbose mode")
)

func _MDConvZhScene(content string) string {
	re := regexp.MustCompile("(?m)^……+")

	return re.ReplaceAllString(content, "---")
}

func _decodeMosaic(content string) string {
	var words [][2]string

	words = [][2]string{
		{"十之**", "十之八九"},
		{"十有**", "十有八九"},
		{"**不离十", "八九不离十"},
		{"**裸", "赤裸裸"},
		{"**力", "大法力"},
		{"求生**", "求生欲望"},
		{"购买**", "购买欲望"},
		{"**份", "失身份"},
		{"高*", "高潮"},

		{"又又", "双"},
		{"强女干", "强奸"},

		{"zheng府", "政府"},
		{"shi长", "市长"},
		{"sheng长", "省长"},
		{"bu长", "部长"},
		{"shou长", "首长"},
		{"首zhang", "首长"},
		{"鸦pian", "鸦片"},
		{"du品", "毒品"},
		{"dan疼", "蛋疼"},
		{"扯dan", "扯蛋"},
		{"嫖chang", "嫖娼"},
		{"开qiang", "开枪"},
		{"洗qian", "洗钱"},
		{"床shang", "床上"},
		{"quan力", "权力"},
		{"gan部", "干部"},
		{"欲wang", "欲望"},
		{"国fang", "国防"},
		{"暴luan", "暴乱"},
		{"通jian", "通奸"},
		{"shuang修", "双修"},
	}

	for _, v := range words {
		content = strings.ReplaceAll(content, v[0], v[1])
	}

	return content
}

func _MDMarkChineseChapter(content string) string {
	ch_level := 1

	if strings.Index(content, "第一卷") > 0 {
		fmt.Println("found part")
		ch_level++

		part_re := regexp.MustCompile("(?m)^ *第([零一二三四五六七八九十两百千]{1,3})卷 *([^。\n]{0,40})$")
		content = part_re.ReplaceAllString(content, "\n# 第${1}卷 $2")
	}

	ch_re := regexp.MustCompile("(?m)^ *([第章]) *([0-9零一二三四五六七八九十两百千]{1,8}) *([章节回幕]) *([^。\n]{0,40})$")

	return ch_re.ReplaceAllString(content, "\n"+strings.Repeat("#", ch_level)+" $1$2$3 $4")
}

func _MDFixHeadlineSpacing(content string) string {
	content = regexp.MustCompile("([^\n])\n+#").ReplaceAllString(content, "$1\n\n#")
	return regexp.MustCompile("(?m)^#([^\n]+)\n+([^\n])").ReplaceAllString(content, "#$1\n\n$2")
}

func _convertLeadingSpaceToNewLine(content string) string {
	re := regexp.MustCompile("(?m)^ +")

	return re.ReplaceAllString(content, "\n")
}

func _convertFullwidthToHalfwidth(content string) string {
	var x map[string]string

	x = map[string]string{
		"　": " ",
		"．": ".",

		"０": "0", "１": "1", "２": "2", "３": "3", "４": "4",
		"５": "5", "６": "6", "７": "7", "８": "8", "９": "9",

		"ａ": "a", "ｂ": "b", "ｃ": "c", "ｄ": "d", "ｅ": "e",
		"ｆ": "f", "ｇ": "g", "ｈ": "h", "ｉ": "i", "ｊ": "j",
		"ｋ": "k", "ｌ": "l", "ｍ": "m", "ｎ": "n", "ｏ": "o",
		"ｐ": "p", "ｑ": "q", "ｒ": "r", "ｓ": "s", "ｔ": "t",
		"ｕ": "u", "ｖ": "v", "ｗ": "w", "ｘ": "x", "ｙ": "y",
		"ｚ": "z",

		"Ａ": "A", "Ｂ": "B", "Ｃ": "C", "Ｄ": "D", "Ｅ": "E",
		"Ｆ": "F", "Ｇ": "G", "Ｈ": "H", "Ｉ": "I", "Ｊ": "J",
		"Ｋ": "K", "Ｌ": "L", "Ｍ": "M", "Ｎ": "N", "Ｏ": "O",
		"Ｐ": "P", "Ｑ": "Q", "Ｒ": "R", "Ｓ": "S", "Ｔ": "T",
		"Ｕ": "U", "Ｖ": "V", "Ｗ": "W", "Ｘ": "X", "Ｙ": "Y",
		"Ｚ": "Z",
	}

	for k, v := range x {
		content = strings.Replace(content, k, v, -1)
	}

	return content
}

func _dos2unix(content string) string {
	return strings.Replace(content, "\r\n", "\n", -1)
}

func _removeEmptyLine(content string) string {
	re := regexp.MustCompile("\n{3,}")

	return re.ReplaceAllString(content, "\n\n")
}

func convertChineseChapterNoToArabic(content string) string {
	re := regexp.MustCompile("第([零一二两三四五六七八九十百千万]+)章")

	replace := func(from string) string {
		return fmt.Sprintf("第%d章",
			chinese_number_to_arabic(re.FindStringSubmatch(from)[1]))
	}

	return re.ReplaceAllStringFunc(content, replace)
}

func _bookInit(in_filename string, out_filename string) {
	if *utf8_conv_flag {
		cmd := exec.Command("any2utf8", in_filename)

		if err := cmd.Run(); err != nil {
			fmt.Println("Error: ", err)
			return
		}
	}

	content := fc.ReadFile(in_filename)

	content = _dos2unix(content)
	content = _convertFullwidthToHalfwidth(content)

	content = _convertLeadingSpaceToNewLine(content)

	if strings.HasSuffix(out_filename, ".md") {
		if *chinese_mode {
			content = _MDMarkChineseChapter(content)
			content = convertChineseChapterNoToArabic(content)
			content = _MDConvZhScene(content)
			content = _decodeMosaic(content)
		}

		content = _MDFixHeadlineSpacing(content)
	}

	content = _removeEmptyLine(content)

	fc.WriteFile(out_filename, content)
}

func chinese_number_to_arabic(str string) int {
	al := map[rune]int{
		'零': 0, '一': 1, '二': 2, '两': 2, '三': 3, '四': 4,
		'五': 5, '六': 6, '七': 7, '八': 8, '九': 9, '十': 10,
		'百': 100, '千': 1000, '万': 10000,
	}

	ret := 0
	n := 0

	for _, rune := range str {
		v := al[rune]

		if v < 10 {
			n = n*10 + v
		} else {
			if n == 0 && v == 10 {
				n = 1
			}

			ret += n * v
			n = 0
		}
	}

	return ret + n
}

func main() {
	if len(os.Args) == 1 {
		fc.Usage()
	}

	flag.Parse()

	for _, x := range flag.Args() {
		fmt.Printf("init %s\n", x)
		_bookInit(x, x)
	}
}
