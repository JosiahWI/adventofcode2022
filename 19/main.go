// Solves Advent of Code day 19.
package main

import (
	"bufio"
	"flag"
	"fmt"
	"io"
	"os"
	"runtime/pprof"
	"strconv"
	"strings"
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

var cpuprofile = flag.String("cpuprofile", "", "write cpu profile to file")
var memprofile = flag.String("memprofile", "", "write memory profile to file")

func main() {
	flag.Parse()
	if *cpuprofile != "" {
		f, err := os.Create(*cpuprofile)
		if err != nil {
			fmt.Fprintf(os.Stderr, "day 19: %v\n", err)
		}
		pprof.StartCPUProfile(f)
		defer pprof.StopCPUProfile()
	}
	for _, arg := range flag.Args() {
		f, err := os.Open(arg)
		if err != nil {
			fmt.Fprintf(os.Stderr, "day 19: %v\n", err)
			continue
		}
		blueprints := parse(f)
		f.Close()
		nblueprints := len(blueprints) / blueprintSize
		product := 1
		for i := 0; i < nblueprints; i++ {
			geodes := optimize(blueprints[blueprintSize*i : blueprintSize*(i+1)])
			fmt.Printf("%d\t%d\n", i, geodes)
			product *= geodes
		}
		fmt.Printf("%d\n", product)
		if *memprofile != "" {
			f, err := os.Create(*memprofile)
			if err != nil {
				fmt.Fprintf(os.Stderr, "day19: %v\n", err)
			}
			pprof.WriteHeapProfile(f)
			f.Close()
			return
		}
	}
}

// Parses modded input (see strip_input.py) into an array of all the costs.
func parse(r io.Reader) []int {
	input := bufio.NewScanner(r)
	if !input.Scan() {
		fmt.Fprint(os.Stderr, "day 19: no input\n")
		return []int{}
	}
	nblueprints := 3
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
	const minutes = 28
	maxore := max(blueprint[0],
		max(blueprint[3], max(blueprint[6], blueprint[9])))
	maxclay := blueprint[7]
	maxoby := blueprint[11]
	var maxn int
	var dfs func(inv state, count int) int
	dfs = func(inv state, count int) int {
		if count > minutes {
			return 0
		}
		if rem := minutes - count; inv.geod+inv.Δgeod*(rem)+rem*(rem-1)/2 > maxn && count != minutes {
			var g0, g1, g2, g3 int
			if inv.Δoby > 0 {
				g0 = dfs(skip(blueprint, inv, count, 3))
			}
			if inv.Δoby < maxoby && inv.Δclay > 0 {
				g1 = dfs(skip(blueprint, inv, count, 2))
			}
			if inv.Δclay < maxclay {
				g2 = dfs(skip(blueprint, inv, count, 1))
			}
			if inv.Δore < maxore {
				g3 = dfs(skip(blueprint, inv, count, 0))
			}
			return max(g0, max(g1, max(g2, g3)))
		}
		return inv.geod + inv.Δgeod*(minutes-count)
	}
	return dfs(state{0, 0, 0, 0, 1, 0, 0, 0}, 0)
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
