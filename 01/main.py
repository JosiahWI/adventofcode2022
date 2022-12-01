#!/usr/bin/env python3

import itertools

def from_input():
    with open("input.txt", "r") as f:
        inventories = [
            [c for c in list(y) if c != "\n"]
            for x, y in itertools.groupby(f.readlines(), lambda l: l == "\n")
        ]
    for inv in inventories:
        yield [int(calories) for calories in inv]

if __name__ == "__main__":
        top_three = list(sorted(map(sum, from_input()), reverse=True))[:3]
        print(top_three)
        print(sum(top_three))
