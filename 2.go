package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
)

func main() {
	file, err := os.Open(os.Args[1])
	if err != nil {
		fmt.Println(err)
		os.Exit(1)
	}

	wrapping := 0
	ribbon := 0
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		line := scanner.Text()

		sides := strings.Split(line, "x")
		l, err := strconv.Atoi(sides[0])
		if err != nil {
			fmt.Println(err)
			os.Exit(1)
		}

		w, err := strconv.Atoi(sides[1])
		if err != nil {
			fmt.Println(err)
			os.Exit(1)
		}

		h, err := strconv.Atoi(sides[2])
		if err != nil {
			fmt.Println(err)
			os.Exit(1)
		}

		wrapping += 2*l*w + 2*w*h + 2*l*h + min(l*w, w*h, l*h)
		ribbon += min(2*(l+w), 2*(w+h), 2*(l+h)) + l*w*h
	}

	fmt.Println(wrapping, ribbon)
}

func min(values ...int) int {
	var min int = values[0]
	for _, v := range values {
		if v < min {
			min = v
		}
	}
	return min
}
