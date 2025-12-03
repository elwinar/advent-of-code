package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
)

func main() {
	raw, err := os.Open("./1.input")
	if err != nil {
		panic(err)
	}

	dial := int64(50)
	password := 0
	s := bufio.NewScanner(raw)
	s.Split(bufio.ScanLines)
	for s.Scan() {
		line := s.Text()
		rot, err := strconv.ParseInt(line[1:], 10, 64)
		if err != nil {
			panic(err)
		}
		if line[0] == 'L' {
			dial -= rot
		} else {
			dial += rot
		}
		dial = dial % 100
		if dial < 0 {
			dial += 100
		}
		if dial == 0 {
			password += 1
		}
		fmt.Println(line, dial, password)
	}
	if s.Err() != nil {
		panic(err)
	}
}
