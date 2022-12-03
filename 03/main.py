#!/usr/bin/env python3

from more_itertools import grouper
from typing import Tuple

def invalid_item(rucksack: str) -> str:
    already_contained = set(sack[:len(sack) // 2])
    for item in sack[len(sack) // 2:]:
        if item in already_contained:
            return item
    raise RuntimeError(f"No invalid item in {rucksack}!")

def shared_item(group: Tuple[str, ...]) -> str:
    fst_contained = set(group[0])
    snd_contained = set(group[1])
    for item in group[2]:
        if item in fst_contained and item in snd_contained:
            return item
    raise RuntimeError(f"No shared item in {group}!")

def points(item: str) -> int:
    if item.islower():
        return ord(item) - ord('a') + 1
    else:
        return ord(item) - ord('A') + 27

if __name__ == "__main__":
    with open("input.txt") as f:
        rucksacks = [line.rstrip() for line in f.readlines()]

    part_one_points = 0
    part_two_points = 0
    for group in grouper(rucksacks, 3):
        part_two_points += points(shared_item(group))
        for sack in group:
            part_one_points += points(invalid_item(sack))

    print(part_one_points)
    print(part_two_points)
