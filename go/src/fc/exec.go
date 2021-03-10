package fc

import (
	"os"
	"os/exec"
)

func PipeCmd(cmds []*exec.Cmd) {
	var prev *exec.Cmd

	cmds[0].Stdin = os.Stdin
	for _, cmd := range cmds {
		cmd.Stderr = os.Stderr

		if prev != nil {
			cmd.Stdin, _ = prev.StdoutPipe()
		}

		prev = cmd
	}

	prev.Stdout = os.Stdout

	for _, cmd := range cmds {
		cmd.Start()
	}

	for _, cmd := range cmds {
		cmd.Wait()
	}
}

func Pipe(commands [][]string) {
	cmds := make([]*exec.Cmd, len(commands))

	for n, x := range commands {
		cmds[n] = exec.Command(x[0], x[1:]...)
	}

	PipeCmd(cmds)
}
