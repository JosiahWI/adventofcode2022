#!/usr/bin/env python3

if __name__ == "__main__":
    with open("input.txt") as f:
        lines = f.readlines()

    total_part_one = 0
    total_part_two = 0
    for line in lines:
        first, second = [part.split("-") for part in line.split(",")]
        if int(first[0]) >= int(second[0]) \
                and int(first[1]) <= int(second[1]):
            total_part_one += 1
        elif int(first[0]) <= int(second[0]) \
                and int(first[1]) >= int(second[1]):
            total_part_one += 1
        if len(set(range(int(first[0]), int(first[1]) + 1)) \
                & set(range(int(second[0]), int(second[1]) + 1))) > 0:
            total_part_two += 1
    print(total_part_one)
    print(total_part_two)
