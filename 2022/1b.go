package main

import (
	"bufio"
	"bytes"
	"fmt"
	"os"
	"strconv"
)

func insert(s []int64, v int64) {
	if s[len(s)-1] > v {
		return
	}

	var i int
	for i = len(s) - 1; i > 0 && s[i-1] < v; i-- {
		s[i] = s[i-1]
	}
	s[i] = v
	return
}

func main() {
	raw, err := os.ReadFile("1.input")
	if err != nil {
		panic(err)
	}

	s := bufio.NewScanner(bytes.NewReader(raw))
	s.Split(bufio.ScanLines)

	var elves = []int64{0, 0, 0}

	var current int64 = 0
	for s.Scan() {
		line := s.Text()
		if len(line) == 0 {
			insert(elves, current)
			current = 0
			continue
		}

		n, err := strconv.ParseInt(s.Text(), 10, 64)
		if err != nil {
			panic(err)
		}

		current += n
	}
	insert(elves, current)

	var total int64 = 0
	for i := 0; i < len(elves); i++ {
		total += elves[i]
	}

	fmt.Println(total)
}
