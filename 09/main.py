#!/usr/bin/env python3

def get_corrected_tail(head_pos, tail_pos):
    print(head_pos, tail_pos)
    need_realign = False
    if head_pos[0] != tail_pos[0] and head_pos[1] != tail_pos[1]:
        need_realign = True

    if head_pos[1] - tail_pos[1] > 1:
        tail_pos = (tail_pos[0], tail_pos[1] + 1)
        if need_realign:
            if head_pos[0] > tail_pos[0]:
                tail_pos = (tail_pos[0] + 1, tail_pos[1])
            else:
                tail_pos = (tail_pos[0] - 1, tail_pos[1])
    elif head_pos[0] - tail_pos[0] < -1:
        tail_pos = (tail_pos[0] - 1, tail_pos[1])
        if need_realign:
            if head_pos[1] > tail_pos[1]:
                tail_pos = (tail_pos[0], tail_pos[1] + 1)
            else:
                tail_pos = (tail_pos[0], tail_pos[1] - 1)
    elif head_pos[1] - tail_pos[1] < -1:
        tail_pos = (tail_pos[0], tail_pos[1] - 1)
        if need_realign:
            if head_pos[0] > tail_pos[0]:
                tail_pos = (tail_pos[0] + 1, tail_pos[1])
            else:
                tail_pos = (tail_pos[0] - 1, tail_pos[1])
    elif head_pos[0] - tail_pos[0] > 1:
        tail_pos = (tail_pos[0] + 1, tail_pos[1])
        if need_realign:
            if head_pos[1] > tail_pos[1]:
                tail_pos = (tail_pos[0], tail_pos[1] + 1)
            else:
                tail_pos = (tail_pos[0], tail_pos[1] - 1)

    return tail_pos

if __name__ == "__main__":
    with open("input.txt", "r") as f:
        expanded_moves = "".join(
            move_c * int(count_str)
            for move_c, count_str in (line.split() for line in f.readlines())
        )

    rope = [(0, 0)] * 10
    visited_part_one = set((rope[1],))
    visited_part_two = set((rope[-1],))
    for move_c in expanded_moves:
        head_pos = rope[0]
        match move_c:
            case "U":
                head_pos = (head_pos[0], head_pos[1] + 1)
            case "L":
                head_pos = (head_pos[0] - 1, head_pos[1])
            case "D":
                head_pos = (head_pos[0], head_pos[1] - 1)
            case "R":
                head_pos = (head_pos[0] + 1, head_pos[1])
        rope[0] = head_pos

        # the heads changing during iteration affect how the tails update
        for i in range(1, len(rope)):
            rope[i] = get_corrected_tail(rope[i - 1], rope[i])
        
        visited_part_one.add(rope[1])
        visited_part_two.add(rope[-1])

    print(rope)
    print(len(visited_part_one))
    print(len(visited_part_two))
