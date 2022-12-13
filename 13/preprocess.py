#!/usr/bin/env python3

import re
import sys

if __name__ == "__main__":
    filename = sys.argv[1]

    with open(f"{filename}") as f:
        lines = f.readlines()

    converted = []
    for line in lines:
        with_simple = re.sub(r"(\d+)", lambda m: f"Simple {m.groups()[0]}", line)
        with_compound = re.sub(r"\[", "Compound [", with_simple)
        converted.append(with_compound)

    with open(f"modded_{filename}", "w") as f:
        f.write("".join(converted))
