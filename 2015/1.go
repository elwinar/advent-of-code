package main

import (
	"fmt"
	"io/ioutil"
	"os"
)

func main() {
	raw, err := ioutil.ReadFile(os.Args[1])
	if err != nil {
		fmt.Println(err)
		os.Exit(1)
	}

	var floor = 0
	var position = 0
	var entered = false
	for _, r := range string(raw) {
		if !entered {
			position += 1
		}

		switch r {
		case '(':
			floor += 1
		case ')':
			floor -= 1
		default:
			fmt.Println("invalid input")
			os.Exit(1)
		}

		if floor < 0 {
			entered = true
		}
	}

	fmt.Println(floor, position)
}
