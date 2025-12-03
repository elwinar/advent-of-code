package main

import (
	"bufio"
	"bytes"
	"fmt"
	"os"
	"strconv"
	"strings"
)

func main() {
	raw, err := os.Open("./2.input")
	if err != nil {
		panic(err)
	}

	answer := 0

	s := bufio.NewScanner(raw)
	s.Split(SplitAny(","))
	for s.Scan() {
		line := strings.TrimSpace(s.Text())
		segments := strings.Split(line, "-")

		start, err := strconv.Atoi(segments[0])
		if err != nil {
			panic(err)
		}

		end, err := strconv.Atoi(segments[1])
		if err != nil {
			panic(err)
		}

		for i := start; i <= end; i += 1 {
			if IsRepeating(i) {
				answer += i
			}
		}
	}
	if s.Err() != nil {
		panic(err)
	}
	fmt.Println(answer)
}

func SplitAny(cutset string) bufio.SplitFunc {
	return func(data []byte, atEOF bool) (advance int, token []byte, err error) {
		if atEOF && len(data) == 0 {
			return 0, nil, nil
		}
		if i := bytes.IndexAny(data, cutset); i >= 0 {
			return i + 1, data[0:i], nil
		}
		if atEOF {
			return len(data), data, nil
		}
		return 0, nil, nil
	}
}

func IsRepeating(i int) bool {
	s := strconv.Itoa(i)
	for c := 2; c <= len(s); c += 1 {
		if len(s)%c != 0 {
			continue
		}
		n := len(s) / c
		if s == strings.Repeat(s[:n], c) {
			return true
		}
	}
	return false
}
