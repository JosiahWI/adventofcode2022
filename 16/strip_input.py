#!/usr/bin/env python3

import re
import sys

valve_regex = re.compile(r"([A-Z]{2}|\d+)")

def encode_valve_name(name: str) -> int:
    def encode(name: str) -> int:
        return 26 * ord(name[0]) + ord(name[1])

    assert len(name) == 2
    encoded = encode(name)
    assert encode("AA") <= encoded <= encode("ZZ")
    return encoded

if __name__ == "__main__":
    if len(sys.argv) < 2:
        print("usage: pack_input.py <source>")
        sys.exit(0)

    source_path = sys.argv[1]
    with open(source_path) as f:
        contents = f.read().splitlines()

    for line in contents:
        name, flow, *connections = valve_regex.findall(line)
        print(
            encode_valve_name(name),
            flow,
            *[encode_valve_name(v) for v in connections]
        )
        
