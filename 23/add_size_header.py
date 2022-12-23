#!/usr/bin/env python3

import sys

if __name__ == "__main__":
    filename = sys.argv[1]
    with open(filename) as f:
        lines = f.readlines()

    n_elves = sum(line.count("#") for line in lines)
    print(n_elves, "".join(lines), end="", sep="\n")
