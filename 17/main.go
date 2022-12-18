package main

import (
	"fmt"
	"io/ioutil"
	"os"
	"strings"
)

type Point struct {
	x, y int
}

type State struct {
	i, j int
}

var shapes = [][]Point{
	{Point{0, 0}, Point{1, 0}, Point{2, 0}, Point{3, 0}},
	{Point{1, 0}, Point{0, 1}, Point{1, 1}, Point{2, 1}, Point{1, 2}},
	{Point{0, 0}, Point{1, 0}, Point{2, 0}, Point{2, 1}, Point{2, 2}},
	{Point{0, 0}, Point{0, 1}, Point{0, 2}, Point{0, 3}},
	{Point{0, 0}, Point{1, 0}, Point{0, 1}, Point{1, 1}}}

func main() {
	files := os.Args[1:]
	for _, filename := range files {
		data, err := ioutil.ReadFile(filename)
		if err != nil {
			fmt.Fprintf(os.Stderr, "day17: %v\n", err)
			continue
		}
		if len(data) == 0 {
			fmt.Fprintf(os.Stderr, "day17: empty file %s\n", filename)
			continue
		}

		height := simulate(strings.TrimRight(string(data), "\n"))
		fmt.Printf("%d\t%s\n", height, filename)
	}
}

func simulate(airStream string) int64 {
	airStreamLength := len(airStream)
	filled := make(map[Point]bool)
	seen := make(map[State]int)
	seenAt := make(map[State]int)
	iters := make(map[State]int64)
	base := 0
	skipped := int64(0)
	j := 0
	for i := int64(0); i < 1000000000000; i++ {
		state := State{int(i % 5), j % airStreamLength}
		seen[state]++

		if seen[state] > 100 && skipped == 0 {
			height := int64(base - seenAt[state])
			cycles := i - iters[state]
			remaining := 1000000000000 - i
			nc := remaining/cycles - 1
			i += nc * cycles
			skipped = nc * height
		}

		seenAt[state] = base
		iters[state] = i

		x, y := 2, base+3
		stopped := false
		for ; !stopped; j++ {
			testx := x
			switch dir := airStream[j%airStreamLength]; dir {
			case '>':
				testx = x + 1
			case '<':
				testx = x - 1
			default:
				fmt.Fprintf(os.Stderr, "day17: bad input %c\n", dir)
			}

			stoppedSide := false
			for _, point := range shapes[i%5] {
				pointXAfter := point.x + testx
				if pointXAfter < 0 || pointXAfter > 6 {
					stoppedSide = true
					break
				}

				if filled[Point{testx + point.x, y + point.y}] {
					stoppedSide = true
					break
				}
			}

			if !stoppedSide {
				x = testx
			}

			if y == 0 {
				stopped = true
			} else {
				for _, point := range shapes[i%5] {
					if filled[Point{x + point.x, y + point.y - 1}] {
						stopped = true
						break
					}
				}
			}

			if !stopped {
				y--
			}
		}

		shapeHeight := 1
		for _, point := range shapes[i%5] {
			filled[Point{x + point.x, y + point.y}] = true
			if point.y+1 > shapeHeight {
				shapeHeight = point.y + 1
			}
		}
		base = max(base, y+shapeHeight)
	}
	return int64(base) + skipped
}

func max(a, b int) int {
	if a > b {
		return a
	}
	return b
}
