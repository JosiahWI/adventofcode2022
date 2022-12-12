#!/usr/bin/env python3

from typing import Iterable, List, NamedTuple, Optional, Set

class Coord(NamedTuple):
    x: int
    y: int

def get_adj(coord: Coord) -> Iterable[Coord]:
    for x_off, y_off in ((0, 1), (1, 0), (0, -1), (-1, 0)):
        yield (Coord(coord.x + x_off, coord.y + y_off))

class Node:

    def __init__(self, height: str, distance: Optional[int] = None) -> None:
        assert "a" <= height <= "z" and (distance is None or distance >= 0)
        self.height = height
        self._distance = distance

    @property
    def distance(self) -> Optional[int]:
        return self._distance

    @distance.setter
    def distance(self, distance: Optional[int]) -> None:
        assert distance is None or distance >= 0
        self._distance = distance

class Edge(NamedTuple):
    origin: Node
    target: Node

def walkable(edge: Edge) -> bool:
    return ord(edge.target.height) - ord(edge.origin.height) <= 1

class Grid:

    def __init__(self, grid: List[List[Node]]):
        if grid:
            assert all(len(row) == len(grid[0]) for row in grid)
            self.width = len(grid[0])
        else:
            self.width = 0

        self.grid = grid
        self.height = len(grid)

    def __getitem__(self, coord: Coord) -> Node:
        return self.grid[coord.y][coord.x]

    def in_bounds(self, coord: Coord) -> bool:
        return 0 <= coord.x < self.width and 0 <= coord.y < self.height

def shortest_path(grid: Grid, start: Coord, end: Coord) -> Optional[int]:
    # does the grid have something in it?
    assert grid
    assert(grid.in_bounds(start) and grid.in_bounds(end))

    visited: Set[Coord] = set()
    visit_next: Set[Coord] = set([start])

    distance = 1
    while len(visit_next) > 0:
        visit_next_next = set()
        for coord in visit_next:
            for adj in get_adj(coord):
                if adj not in visited \
                        and adj not in visit_next \
                        and grid.in_bounds(adj) \
                        and walkable(Edge(grid[coord], grid[adj])):
                    grid[adj].distance = distance
                    visit_next_next.add(adj)
        visited |= visit_next
        visit_next = visit_next_next
        distance += 1

    return grid[end].distance

def shortest_path_2(grid: Grid, end: Coord) -> Optional[int]:
    # does the grid have something in it?
    assert grid
    assert(grid.in_bounds(end))

    shortest_distance: Optional[int] = None
    visited: Set[Coord] = set()
    visit_next: Set[Coord] = set([end])

    distance = 1
    while len(visit_next) > 0:
        visit_next_next = set()
        for coord in visit_next:
            for adj in get_adj(coord):
                if adj not in visited \
                        and adj not in visit_next \
                        and grid.in_bounds(adj) \
                        and walkable(Edge(grid[adj], grid[coord])):
                    grid[adj].distance = distance
                    if grid[adj].height == "a" and shortest_distance is None:
                        shortest_distance = distance
                    visit_next_next.add(adj)
        visited |= visit_next
        visit_next = visit_next_next
        distance += 1

    return shortest_distance


if __name__ == "__main__":
    grid: List[List[Node]] = []
    start_pos: Optional[Coord] = None
    end_pos: Optional[Coord] = None
    with open("input.txt") as f:
        for y, column in enumerate(f.read().splitlines()):
            row = []
            for x, height in enumerate(column):
                if height == 'S':
                    row.append(Node('a', 0))
                    start_pos = Coord(x, y)
                elif height == 'E':
                    row.append(Node('z'))
                    end_pos = Coord(x, y)
                else:
                    row.append(Node(height))
            grid.append(row)

    if start_pos is not None and end_pos is not None:
        if all(len(row) == len(grid[0]) for row in grid):
            print(shortest_path(Grid(grid), start_pos, end_pos))
            print(shortest_path_2(Grid(grid), end_pos))
        else:
            print("grid rows are not all the same length")
    else:
        print("start or end pos missing from input")
