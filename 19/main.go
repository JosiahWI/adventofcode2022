// Solves Advent of Code day 19.
package main

import (
	"bufio"
	"fmt"
	"io"
	"os"
	"strconv"
	"strings"
	"sync"
)

const (
	blueprintSize = 12
)

type state struct {
	ore, clay, oby, geod, Δore, Δclay, Δoby, Δgeod int
}

func (s *state) update() {
	s.ore += s.Δore
	s.clay += s.Δclay
	s.oby += s.Δoby
	s.geod += s.Δgeod
}

func max(a, b int) int {
	if a > b {
		return a
	}
	return b
}

func main() {
	for _, arg := range os.Args[1:] {
		f, err := os.Open(arg)
		if err != nil {
			fmt.Fprintf(os.Stderr, "day 19: %v\n", err)
			continue
		}
		blueprints := parse(f)
		f.Close()
		nblueprints := len(blueprints) / blueprintSize
		sum := 0
		for i := 0; i < nblueprints; i++ {
			quality := (i + 1) * optimize(blueprints[blueprintSize*i:blueprintSize*(i+1)])
			fmt.Printf("%d\t%d\n", i, quality)
			sum += quality
		}
		fmt.Printf("%d\n", sum)
	}
}

// Parses modded input (see strip_input.py) into an array of all the costs.
func parse(r io.Reader) []int {
	input := bufio.NewScanner(r)
	if !input.Scan() {
		fmt.Fprint(os.Stderr, "day 19: no input\n")
		return []int{}
	}
	nblueprints, err := strconv.Atoi(input.Text())
	if err != nil {
		fmt.Fprintf(os.Stderr, "day19: reading header: %v\n", err)
	}
	var blueprints []int
	for i := 0; i < nblueprints; i++ {
		for j := 0; j < 4; j++ {
			if !input.Scan() {
				fmt.Fprint(os.Stderr, "day19: input terminated early\n")
			}
			words := strings.Split(input.Text(), " ")
			if len(words) != 3 {
				fmt.Fprint(os.Stderr, "day19: wrong number of quantities\n")
			}
			for _, word := range words {
				qty, err := strconv.Atoi(word)
				if err != nil {
					fmt.Fprintf(os.Stderr, "day19: reading quantity: %v\n", err)
					return []int{}
				}
				blueprints = append(blueprints, qty)
			}
		}
	}
	return blueprints
}

// Uses DFS to find the optimal number of geodes cracked.
func optimize(blueprint []int) int {
	const minutes = 24
	maxore := max(blueprint[0],
		max(blueprint[3], max(blueprint[6], blueprint[9])))
	maxclay := blueprint[7]
	maxoby := blueprint[11]
	ch := make(chan int)
	var wc sync.WaitGroup
	var maxn int
	var dfs func(inv state, count int)
	dfs = func(inv state, count int) {
		if count > minutes {
			wc.Done()
			return
		}
		rem := minutes - count
		if count != minutes && inv.geod+inv.Δgeod*(rem)+rem*(rem-1)/2 > maxn {
			if inv.Δoby > 0 {
				wc.Add(1)
				go dfs(skip(blueprint, inv, count, 3))
			}
			if inv.Δoby < maxoby && inv.Δclay > 0 {
				wc.Add(1)
				go dfs(skip(blueprint, inv, count, 2))
			}
			if inv.Δclay < maxclay {
				wc.Add(1)
				go dfs(skip(blueprint, inv, count, 1))
			}
			if inv.Δore < maxore {
				wc.Add(1)
				go dfs(skip(blueprint, inv, count, 0))
			}
		}
		ch <- inv.geod + inv.Δgeod*(minutes-count)
		wc.Done()
		if count == 0 {
			wc.Wait()
			close(ch)
		}
	}
	wc.Add(1)
	go dfs(state{0, 0, 0, 0, 1, 0, 0, 0}, 0)
	for n := range ch {
		if n > maxn {
			maxn = n
		}
	}
	return maxn
}

// Skip waiting turns and buy the target robot.
func skip(blueprint []int, inv state, count, i int) (state, int) {
	skipped := 0
	after := state{inv.ore, inv.clay, inv.oby, inv.geod, inv.Δore, inv.Δclay, inv.Δoby, inv.Δgeod}
	for after.ore < blueprint[3*i] || after.clay < blueprint[3*i+1] || after.oby < blueprint[3*i+2] {
		skipped++
		after.update()
	}
	after.update()
	skipped++
	switch i {
	case 0:
		after.ore -= blueprint[0]
		after.Δore++
	case 1:
		after.ore -= blueprint[3]
		after.Δclay++
	case 2:
		after.ore -= blueprint[6]
		after.clay -= blueprint[7]
		after.Δoby++

	case 3:
		after.ore -= blueprint[9]
		after.oby -= blueprint[11]
		after.Δgeod++
	}
	return after, count + skipped
}
