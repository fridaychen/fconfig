package fc

import (
	"flag"
	"fmt"
	"os"
)

type ArrayFlags []string

func (i *ArrayFlags) String() string {
	return ""
}

func (i *ArrayFlags) Set(value string) error {
	*i = append(*i, value)

	return nil
}

func Var(argName, description string) *ArrayFlags {
	var arg ArrayFlags

	flag.Var(&arg, argName, description)

	return &arg
}

func Usage() {
	fmt.Println("Usage : " + os.Args[0])

	flag.PrintDefaults()

	os.Exit(0)
}
