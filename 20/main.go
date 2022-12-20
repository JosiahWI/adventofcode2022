// Solves Advent of Code Day 20.
package main

import (
	"fmt"
	"io/ioutil"
	"os"
	"strconv"
	"strings"
)

func main() {
	for _, arg := range os.Args[1:] {
		data, err := ioutil.ReadFile(arg)
		if err != nil {
			fmt.Fprintf(os.Stderr, "day19: %v\n", err)
			continue
		}
		numbers, err := parse(data)
		if err != nil {
			fmt.Fprintf(os.Stderr, "day19: parsing: %v\n", err)
			continue
		}

		view := make([]int, len(numbers))
		for i, _ := range view {
			view[i] = i
		}
		for i := 0; i < 10; i++ {
			mix(numbers, view)
		}
		i, _ := find(numbers, 0)
		base, _ := find(view, i)
		nnumbers := len(view)
		a := numbers[view[(base+1000)%nnumbers]]
		b := numbers[view[(base+2000)%nnumbers]]
		c := numbers[view[(base+3000)%nnumbers]]
		fmt.Printf("(%d)+(%d)+(%d)=(%d)\n", a, b, c, a+b+c)
	}
}

func mix(numbers []int, view []int) {
	for i, n := range numbers {
		j, ok := find(view, i)
		if !ok {
			fmt.Fprintf(os.Stderr, "day20: mix: no %d in view", i)
			os.Exit(1)
		}
		swap(view, mod(j, len(numbers)), mod(j+n, len(numbers)-1))
		show := make([]int, len(numbers))
		for i, j := range view {
			show[i] = numbers[j]
		}
	}
}

func parse(data []byte) ([]int, error) {
	var numbers []int
	for _, line := range strings.Split(strings.Trim(string(data), "\n"), "\n") {
		n, err := strconv.Atoi(line)
		if err != nil {
			return []int{}, err
		}
		numbers = append(numbers, n*811589153)
	}
	return numbers, nil
}

func find(arr []int, q int) (int, bool) {
	for i, n := range arr {
		if n == q {
			return i, true
		}
	}
	return 0, false
}

func swap(arr []int, i, j int) {
	if i < j {
		temp := arr[i]
		copy(arr[i:j], arr[i+1:j+1])
		arr[j] = temp
	} else if i > j {
		temp := arr[i]
		copy(arr[j+1:i+1], arr[j:i])
		arr[j] = temp
	}
}

func mod(a, b int) int {
	return (a%b + b) % b
}
