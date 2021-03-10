package fc

import (
	"os"
)

func FileExists(path string) bool {
	info, err := os.Stat(path)

	return os.IsNotExist(err) == false && info.IsDir() == false
}

func DirExists(path string) bool {
	info, err := os.Stat(path)

	return os.IsNotExist(err) == false && info.IsDir() == true
}
