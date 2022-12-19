package main

import (
	"errors"
	"fmt"
	"io/ioutil"
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

		flood := floodfill(vertices)
		results := make(chan int)
		for _, vert := range vertices {
			go countEdges(vert, flood, results)

		}

		vertexCount := len(vertices)
		surfaceArea := 0
		for i := 0; i < vertexCount; i++ {
			surfaceArea += <-results
		}

		fmt.Printf("%d\t%s\n", surfaceArea, filename)
	}
}

func floodfill(vertices []Vec3d) map[Vec3d]bool {
	var maxx, maxy, maxz int
	cubemap := make(map[Vec3d]bool)
	for _, vert := range vertices {
		cubemap[vert] = true
		if vert.x > maxx {
			maxx = vert.x
		}
		if vert.y > maxy {
			maxy = vert.y
		}
		if vert.z > maxz {
			maxz = vert.z
		}
	}

	maxx, maxy, maxz = maxx+1, maxy+1, maxz+1

	flood := make(map[Vec3d]bool)
	inBounds := func(vert Vec3d) bool {
		return -1 <= vert.x && vert.x <= maxx && -1 <= vert.y && vert.y <= maxy && -1 <= vert.z && vert.z <= maxz
	}

	offsets := []Vec3d{Vec3d{1, 0, 0}, Vec3d{-1, 0, 0}, Vec3d{0, 1, 0}, Vec3d{0, -1, 0}, Vec3d{0, 0, 1}, Vec3d{0, 0, -1}}

	for queue := []Vec3d{Vec3d{-1, -1, -1}}; len(queue) > 0; {
		v := queue[0]
		queue = queue[1:]
		for _, off := range offsets {
			adj := Vec3d{v.x + off.x, v.y + off.y, v.z + off.z}
			if !inBounds(adj) || cubemap[adj] || flood[adj] {
				continue
			}

			flood[adj] = true
			queue = append(queue, adj)
		}
	}

	return flood
}

func countEdges(here Vec3d, flood map[Vec3d]bool, res chan<- int) {
	offsets := []Vec3d{Vec3d{1, 0, 0}, Vec3d{-1, 0, 0}, Vec3d{0, 1, 0}, Vec3d{0, -1, 0}, Vec3d{0, 0, 1}, Vec3d{0, 0, -1}}

	edges := 0
	for _, off := range offsets {
		adj := Vec3d{here.x + off.x, here.y + off.y, here.z + off.z}
		if flood[adj] {
			edges++
		}
	}
	res <- edges
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
