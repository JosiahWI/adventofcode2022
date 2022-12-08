#!/usr/bin/env python3

import math

def count_visible(view, this_height):
    print(view)
    total = 0
    for height in view:
        if height >= this_height:
            total += 1
            break
        total += 1
    print(this_height, total)
    return total

if __name__ == "__main__":
    with open("input.txt", "r") as f:
        contents = f.read()

    forest = [
        [int(digit) for digit in row]
        for row in contents.splitlines()
    ]

    trees_visible = 0
    best_scenic_score = 0
    for this_y, this_row in enumerate(forest):
        for this_x, this_height in enumerate(this_row):
            trees_row_less = [forest[this_y][x] for x in range(0, this_x)][::-1]
            trees_row_greater = [
                forest[this_y][x]
                for x in range(this_x + 1, len(this_row))
            ]
            trees_col_less = [forest[y][this_x] for y in range(0, this_y)][::-1]
            trees_col_greater = [
                forest[y][this_x]
                for y in range(this_y + 1, len(forest))
            ]
            if any((
                all(height < this_height for height in trees_row_less),
                all(height < this_height for height in trees_row_greater),
                all(height < this_height for height in trees_col_less),
                all(height < this_height for height in trees_col_greater)
                    )):
                trees_visible += 1

            scenic_score = math.prod((
                count_visible(trees_row_less, this_height),
                count_visible(trees_row_greater, this_height),
                count_visible(trees_col_less, this_height),
                count_visible(trees_col_greater, this_height)
            ))
            
            if scenic_score > best_scenic_score:
                best_scenic_score = scenic_score

    print(trees_visible)
    print(best_scenic_score)
