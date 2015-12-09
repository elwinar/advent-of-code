package main

import (
	"crypto/md5"
	"fmt"
	"io/ioutil"
	"os"
	"strconv"
	"strings"
)

func main() {
	raw, err := ioutil.ReadFile(os.Args[1])
	if err != nil {
		fmt.Println(err)
		os.Exit(1)
	}
	secret := string(raw)

	size, err := strconv.Atoi(os.Args[2])
	if err != nil {
		fmt.Println(err)
		os.Exit(1)
	}

	i := 0
	sum := fmt.Sprintf("%x", md5.Sum([]byte(secret+strconv.Itoa(i))))
	for !strings.HasPrefix(sum, strings.Repeat("0", size)) {
		i++
		sum = fmt.Sprintf("%x", md5.Sum([]byte(secret+strconv.Itoa(i))))
	}

	fmt.Println(i)
}
