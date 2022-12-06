#!/usr/bin/env python3

import copy

if __name__ == "__main__":
    with open("input.txt") as f:
        contents = f.read()
    initial_stacks, commands = contents.split("\n\n")
    stacks_one = []
    for i in range(1, 9 * 4 + 2, 4):
        stacks_one.append([
            row[i] for row in initial_stacks.split("\n")[-2::-1]
            if i < len(row) and row[i] != ' '
        ])
    stacks_two = copy.deepcopy(stacks_one[:])

    indices = [[int(word) for word in line.split() if word.isdigit()]
               for line in commands.split("\n")]
    for count, from_stack, to_stack in indices[:-1]:
        for _ in range(count):
            stacks_one[to_stack - 1].append(stacks_one[from_stack - 1].pop())
        stacks_two[to_stack - 1].extend(stacks_two[from_stack - 1][-count:])
        del stacks_two[from_stack - 1][-count:]
    print("".join(stack[-1] for stack in stacks_one if stack))
    print("".join(stack[-1] for stack in stacks_two if stack))
    
