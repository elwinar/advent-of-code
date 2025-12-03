package main

import (
	"bufio"
	"fmt"
	"os"
)

func main() {
	raw, err := os.Open("./3.input")
	if err != nil {
		panic(err)
	}

	answer := 0

	s := bufio.NewScanner(raw)
	s.Split(bufio.ScanLines)
	for s.Scan() {
		line := s.Text()

		highest := 0
		for i := 1; i < len(line); i += 1 {
			if line[i] > line[highest] {
				highest = i
			}
		}

		joltage := 0
		if highest == len(line)-1 {
			second := highest - 1
			for i := highest - 2; i > 0; i -= 1 {
				if line[i] > line[second] {
					second = i
				}
			}
			joltage = int(line[second]-'0')*10 + int(line[highest]-'0')
		} else {
			second := highest + 1
			for i := highest + 2; i < len(line); i += 1 {
				if line[i] > line[second] {
					second = i
				}
			}
			joltage = int(line[highest]-'0')*10 + int(line[second]-'0')
		}

		fmt.Println(line, highest)
		answer += joltage
	}
	if s.Err() != nil {
		panic(err)
	}

	fmt.Println(answer)
}
