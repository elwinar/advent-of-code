package main

import (
	"bufio"
	"bytes"
	"fmt"
	"os"
	"strconv"
)

func main() {
	raw, err := os.ReadFile("1.input")
	if err != nil {
		panic(err)
	}

	s := bufio.NewScanner(bytes.NewReader(raw))
	s.Split(bufio.ScanLines)

	var current int64 = 0
	var max int64 = 0
	for s.Scan() {
		line := s.Text()
		if len(line) == 0 {
			if current > max {
				max = current
			}
			current = 0
			continue
		}

		n, err := strconv.ParseInt(s.Text(), 10, 64)
		if err != nil {
			panic(err)
		}

		current += n
	}
	if current > max {
		max = current
	}

	fmt.Println(max)
}
