// Solves Advent of Code Day 23.
package main

import (
	"bufio"
	"fmt"
	"io"
	"os"
	"strconv"
)

var checkOffs = map[Delta][3]Delta{
	Delta{0, -1}: {Delta{-1, -1}, Delta{0, -1}, Delta{1, -1}},
	Delta{0, 1}:  {Delta{1, 1}, Delta{0, 1}, Delta{-1, 1}},
	Delta{-1, 0}: {Delta{-1, 1}, Delta{-1, 0}, Delta{-1, -1}},
	Delta{1, 0}:  {Delta{1, -1}, Delta{1, 0}, Delta{1, 1}}}

var moveOffs = [4]Delta{Delta{0, -1}, Delta{0, 1},
	Delta{-1, 0}, Delta{1, 0}}

type Coord struct {
	x, y int
}

type Delta struct {
	x, y int
}

func rotateOffs() {
	temp := moveOffs[0]
	copy(moveOffs[:3], moveOffs[1:])
	moveOffs[3] = temp
}

func main() {
	for _, arg := range os.Args[1:] {
		f, err := os.Open(arg)
		if err != nil {
			fmt.Fprintf(os.Stderr, "%v\n", err)
			continue
		}
		elves, err := parse(f)
		f.Close()
		if err != nil {
			fmt.Fprintf(os.Stderr, "parsing: %vn", err)
		}
		for i := 0; i < 10; i++ {
			simulate(elves)
			rotateOffs()
		}
		fmt.Printf("%d\n", getEmpty(elves))
	}
}

func simulate(elves map[Coord]bool) {
	nelves := len(elves)
	proposals := make(map[Coord]Coord, nelves)
	counts := make(map[Coord]int, nelves)
	for at, _ := range elves {
		if to, ok := getProposal(elves, at); ok {
			proposals[at] = to
			counts[to]++
		}
	}
	for at, to := range proposals {
		if counts[to] == 1 {
			elves[to] = true
			delete(elves, at)
		}
	}
}

func getProposal(elves map[Coord]bool, at Coord) (Coord, bool) {
	needed := false
	found := false
	var proposal Coord
	for _, off := range moveOffs {
		valid := true
		for _, checkOff := range checkOffs[off] {
			check := Coord{at.x + checkOff.x, at.y + checkOff.y}
			if elves[check] {
				valid = false
				needed = true
				break
			}
		}
		if valid && !found {
			proposal = Coord{at.x + off.x, at.y + off.y}
			found = true
		}
	}
	return proposal, needed && found
}

func getEmpty(elves map[Coord]bool) int {
	var minx, miny, maxx, maxy int
	for at, _ := range elves {
		if at.x < minx {
			minx = at.x
		} else if at.x > maxx {
			maxx = at.x
		}
		if at.y < miny {
			miny = at.y
		} else if at.y > maxy {
			maxy = at.y
		}
	}
	area := (maxx + 1 - minx) * (maxy + 1 - miny)
	return area - len(elves)
}

func parse(r io.Reader) (map[Coord]bool, error) {
	input := bufio.NewScanner(r)
	input.Scan()
	nelves, err := strconv.Atoi(input.Text())
	if err != nil {
		return nil, err
	}
	elves := make(map[Coord]bool, nelves)
	for y := 0; input.Scan(); y++ {
		for x, sym := range input.Text() {
			if sym == '#' {
				elves[Coord{x, y}] = true
			}
		}
	}
	return elves, nil
}
