package fc

import (
	"errors"
	"os"
	"path"
)

func ChdirProjRoot() error {
	current, _ := os.Getwd()

	for {
		if FileExists(current+"/.TOP") || FileExists(current+"/TOP") {
			os.Chdir(current)
			return nil
		}

		if current == "/" {
			return errors.New("Project not found")
		}

		current = path.Dir(current)
	}
}
