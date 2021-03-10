package fc

import (
	"errors"
	"fmt"
	"regexp"
	"strconv"
	"time"
)

func GenHumanSize(n int64) string {
	switch {
	case n < 1024:
		return fmt.Sprintf("%7dB", n)

	case n < 1024*1024:
		return fmt.Sprintf("%7.2fK", float64(n)/1024)

	case n < 1024*1024*1024:
		return fmt.Sprintf("%7.2fM", float64(n)/1024/1024)

	case n < 1024*1024*1024*1024:
		return fmt.Sprintf("%7.2fG", float64(n)/1024/1024/1024)

	default:
		return fmt.Sprintf("%d", n)
	}
}

func ParseHumanSize(s string) (int64, error) {
	subs := regexp.MustCompile(`^(\d+)([kKmMgGtT]?)$`).FindStringSubmatch(s)

	if len(subs) == 0 {
		return 0, errors.New("parse size error")
	}

	size, _ := strconv.ParseInt(subs[1], 10, 64)

	switch subs[2] {
	case "k", "K":
		size *= 1024
	case "m", "M":
		size *= 1024 * 1024
	case "g", "G":
		size *= 1024 * 1024 * 1024
	case "t", "T":
		size *= 1024 * 1024 * 1024 * 1024
	default:
	}

	return size, nil
}

func ParseHumanTime(s string) (time.Time, error) {
	zero := time.Time{}

	if len(s) == 0 {
		return zero, nil
	}

	subs := regexp.MustCompile(`^(\d+)([mhd]?)$`).FindStringSubmatch(s)

	if len(subs) == 0 {
		return zero, errors.New("parse time error")
	}

	n, _ := strconv.ParseInt(subs[1], 10, 64)

	n = -1 * n

	now := time.Now()

	switch subs[2] {
	case "m":
		return now.Add(time.Duration(n) * time.Minute), nil
	case "h":
		return now.Add(time.Duration(n) * time.Hour), nil
	case "d":
		return now.Add(time.Duration(n*24) * time.Hour), nil
	default:
		return now.Add(time.Duration(n*24) * time.Hour), nil
	}
}
