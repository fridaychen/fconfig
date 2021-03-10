package fc

import (
	"fmt"
	"io/ioutil"
)

func ReadFile(filename string) string {
	data, err := ioutil.ReadFile(filename)

	if err != nil {
		fmt.Println("open error")
		return ""
	}

	return string(data)
}

func WriteFile(filename string, str string) {
	ioutil.WriteFile(filename, []byte(str), 0644)
}
