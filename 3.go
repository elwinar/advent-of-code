package main

import (
	"fmt"
	"io/ioutil"
	"os"
	"strconv"
)

func main() {
	raw, err := ioutil.ReadFile(os.Args[1])
	if err != nil {
		fmt.Println(err)
		os.Exit(1)
	}

	number, err := strconv.Atoi(os.Args[2])
	if err != nil {
		fmt.Println(err)
		os.Exit(1)
	}

	workers := make([]struct {
		x, y int
	}, number)
	w := -1

	visited := make(map[int]map[int]struct{})
	visited[0] = make(map[int]struct{})
	visited[0][0] = struct{}{}

	for _, d := range raw {
		w = (w + 1) % number

		switch d {
		case '<':
			workers[w].x--
		case '>':
			workers[w].x++
		case '^':
			workers[w].y--
		case 'v':
			workers[w].y++
		default:
			fmt.Println("unknown direction", d)
			os.Exit(1)
		}

		if _, found := visited[workers[w].x]; !found {
			visited[workers[w].x] = make(map[int]struct{})
		}

		visited[workers[w].x][workers[w].y] = struct{}{}
	}

	count := 0
	for _, t := range visited {
		count += len(t)
	}

	fmt.Println(count)
}
