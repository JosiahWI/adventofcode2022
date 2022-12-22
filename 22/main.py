#!/usr/bin/env python3

# indexed by rotation code
_OFFSETS = [(1, 0), (0, 1), (-1, 0), (0, -1)]

class Game:

    def __init__(self, grid, x, y):
        assert grid
        assert all(len(row) == len(grid[0]) for row in grid)
        self.grid = grid
        self.x = x
        self.y = y
        self.rot = 0

    def in_bounds(self, x, y):
        return 0 <= x < len(self.grid[0]) and 0 <= y < len(self.grid)

    def forward(self, reps):
        x_off, y_off = _OFFSETS[self.rot]
        self.rep_off(x_off, y_off, reps)

    def cw(self):
        self.rot = (self.rot + 1) % 4

    def ccw(self):
        self.rot = (self.rot - 1) % 4

    def rep_off(self, x_off, y_off, reps):
        for _ in range(reps):
            check_x, check_y = self.x + x_off, self.y + y_off
            if not self.in_bounds(check_x, check_y) \
                    or self.grid[check_y][check_x] == " ":
                check_x, check_y = self.wrap(check_x, check_y, x_off, y_off)

            match self.grid[check_y][check_x]:
                case "#":
                    return
                case ".":
                    self.x, self.y = check_x, check_y
                case " ":
                    raise RuntimeError("this better not happen")

    def wrap(self, x, y, x_off, y_off):
        x_off, y_off = -1*x_off, -1*y_off
        while True:
            check_x, check_y = x + x_off, y + y_off
            if not self.in_bounds(check_x, check_y) \
                    or self.grid[check_y][check_x] == " ":
                break
            x, y = check_x, check_y
        return x, y

    def encode(self):
        return 1000 * (self.y+1) + 4*(self.x+1) + self.rot

def first_open(row):
    x = 0
    while row[x] == " ":
        x += 1
    return x

if __name__ == "__main__":
    with open("input.txt") as f:
        lines = f.read().splitlines()

    grid = []
    for line in lines:
        if line == "":
            break
        grid.append(line)

    width = max(len(row) for row in grid)
    fixed = []
    for row in grid:
        fixed.append(row + (" "*(width-len(row))))
    grid = fixed

    moves = lines[-1]

    x, y = first_open(grid[0]), 0
    game = Game(grid, x, y)
    
    moves_spaced = ""
    for c in moves:
        if c == "R" or c == "L":
            moves_spaced += f" {c} "
        else:
            moves_spaced += c
    moves = [int(c) if c.isdigit() else c for c in moves_spaced.split() if c != " "]
    for move in moves:
        if move == "R":
            game.cw()
        elif move == "L":
            game.ccw()
        else:
            game.forward(move)

    print(game.encode())
