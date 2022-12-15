#!/usr/bin/env python3

import itertools
import more_itertools
import sys
from typing import List

SAND_POINT = (500, 0)

if __name__ == "__main__":
    with open("input.txt") as f:
        contents = f.readlines()
    coords = [[[int(c) for c in coord.split(",")] for coord in line.split("->")] for line in contents]

    height = max(pair[1] for pair in more_itertools.flatten(coords))
    width = max(pair[0] for pair in more_itertools.flatten(coords))

    blocked = set()
    for rock_line in coords:
        for source, dest in itertools.pairwise(rock_line):
            if not (source[0] == dest[0] or source[1] == dest[1]):
                raise RuntimeError("rock line is not horizontal or vertical")

            if source[0] == dest[0]:
                for y in range(min(source[1], dest[1]), max(source[1], dest[1]) + 1):
                    blocked.add((source[0], y))
            else:
                for x in range(min(source[0], dest[0]), max(source[0], dest[0]) + 1):
                    blocked.add((x, source[1]))

    count = 0
    part_one_complete = False
    while (500, 0) not in blocked:
        pos = SAND_POINT
        while pos[1] < (1 + height):
            if (next_pos := (pos[0], pos[1] + 1)) not in blocked:
                pos = next_pos
            elif (next_pos := (pos[0] - 1, pos[1] + 1)) not in blocked:
                pos = next_pos
            elif (next_pos := (pos[0] + 1, pos[1] + 1)) not in blocked:
                pos = next_pos
            else:
                blocked.add(pos)
                break
        else:
            blocked.add(pos)

            if not part_one_complete:
                print(count)
                part_one_complete = True

        count += 1

    print(count)
