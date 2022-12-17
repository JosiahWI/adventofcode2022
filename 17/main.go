package main

import (
	"fmt"
	"io/ioutil"
	"os"
	"strings"
)

type Point struct {
	x int
	y int
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

func simulate(airStream string) int {
	airStreamLength := len(airStream)
	filled := make(map[Point]bool)
	base := 0
	j := 0
	for i := 0; i < 2022; i++ {
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

			for _, point := range shapes[i%5] {
				if y == 0 || filled[Point{x + point.x, y + point.y - 1}] {
					stopped = true
					break
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
	return base
}

func max(a, b int) int {
	if a > b {
		return a
	}
	return b
}
