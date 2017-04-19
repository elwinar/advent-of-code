package main

import (
	"bufio"
	"fmt"
	"os"
	"strings"
)

func main() {
	file, err := os.Open(os.Args[1])
	if err != nil {
		fmt.Println(err)
		os.Exit(1)
	}

	bad := []string{
		"ab",
		"cd",
		"pq",
		"xy",
	}
	nice := 0

	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		line := scanner.Text()

		vowels := 0
		doubles := 0
		naughty := false

		var old rune
		for _, c := range line {
			if strings.ContainsRune("aeiou", c) {
				vowels++
			}

			if old == c {
				doubles++
			}

			for _, b := range bad {
				if string([]rune{old, c}) == b {
					naughty = true
				}
			}

			old = c
		}

		if !naughty && vowels >= 3 && doubles >= 1 {
			nice++
		}

	}

	fmt.Println(nice)
}
