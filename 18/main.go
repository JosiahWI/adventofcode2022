package main

import (
	"errors"
	"fmt"
	"io/ioutil"
	"math"
	"os"
	"strconv"
	"strings"
)

type Vec3d struct {
	x, y, z int
}

func main() {
	files := os.Args[1:]
	for _, filename := range files {
		data, err := ioutil.ReadFile(filename)
		if err != nil {
			fmt.Fprintf(os.Stderr, "day18: %v\n", err)
			continue
		}
		if len(data) == 0 {
			fmt.Fprintf(os.Stderr, "day18: empty %s\n", filename)
			continue
		}

		vertices, err := parse(string(data))
		if err != nil {
			fmt.Fprintf(os.Stderr, "day18: %v\n", err)
		}

		results := make(chan int)
		for i, _ := range vertices {
			go countEdges(vertices[i], vertices[i+1:], results)

		}

		vertexCount := len(vertices)
		surfaceArea := 6 * vertexCount
		for i := 0; i < vertexCount; i++ {
			surfaceArea -= <-results
		}

		fmt.Printf("%d\t%s\n", surfaceArea, filename)
	}
}

func countEdges(here Vec3d, others []Vec3d, res chan<- int) {
	edges := 0
	for _, there := range others {
		dx := math.Abs(float64(here.x - there.x))
		dy := math.Abs(float64(here.y - there.y))
		dz := math.Abs(float64(here.z - there.z))
		if dx+dy+dz == 1 {
			edges++
		}
	}
	res <- 2 * edges
}

func parse(data string) ([]Vec3d, error) {
	var vertices []Vec3d
	for _, line := range strings.Split(strings.Trim(string(data), "\n"), "\n") {

		coords := strings.Split(line, ",")
		if len(coords) != 3 {
			fmt.Printf("%v\n", coords)
			return vertices, errors.New("wrong number of coords")
		}
		x, err := strconv.Atoi(coords[0])
		if err != nil {
			return vertices, err
		}

		y, err := strconv.Atoi(coords[1])
		if err != nil {
			return vertices, err
		}

		z, err := strconv.Atoi(coords[2])
		if err != nil {
			return vertices, err
		}

		vertices = append(vertices, Vec3d{x, y, z})
	}

	return vertices, nil
}
