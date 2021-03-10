package fc

import (
	"log"
	"os"
	"runtime/debug"
	"runtime/pprof"
)

func Init() {
	log.SetFlags(log.Lmicroseconds | log.Lshortfile)
}

func StartProfile() {
	f, err := os.Create("cpu.prof")

	if err == nil {
		pprof.StartCPUProfile(f)
	}
}

func StopProfile() {
	pprof.StopCPUProfile()
}

func DisableGC() {
	debug.SetGCPercent(-1)
}

func EnableGC() {
	debug.SetGCPercent(100)
}
