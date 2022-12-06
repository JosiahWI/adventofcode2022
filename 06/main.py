#!/usr/bin/env python3

import more_itertools

def find_start_of_packet(signal: str) -> int:
    byte_count = 0
    for marker in more_itertools.windowed(signal, 4):
        if all(marker.count(x) == 1 for x in marker):
            byte_count += 4
            break
        else:
            byte_count += 1
    return byte_count

def find_start_of_message(signal: str) -> int:
    byte_count = 0
    for marker in more_itertools.windowed(signal, 14):
        if all(marker.count(x) == 1 for x in marker):
            byte_count += 14
            break
        else:
            byte_count += 1
    return byte_count

if __name__ == "__main__":
    with open("input.txt") as f:
        signal = f.read()
    print(find_start_of_packet(signal))
    print(find_start_of_message(signal))

