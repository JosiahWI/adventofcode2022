#!/usr/bin/env python3

import re
import sys

cost_regex = re.compile(r"Each [a-zA-Z]+ robot costs (?:(\d+) ore)?(?: and (\d+) clay)?(?: and (\d+) obsidian)?")

if __name__ == "__main__":
    filename = sys.argv[1]
    with open(filename) as f:
        lines = f.readlines()

    print(len(lines))
    for line in lines:
        for bot in line[line.index("E"):].split(". "):
            match = cost_regex.match(bot)
            if match is None:
                raise RuntimeError(f"could not match {bot}")
            ore, clay, obsidian = match.groups()
            if clay is None:
                clay = 0
            if obsidian is None:
                obsidian = 0
            print(ore, clay, obsidian)

