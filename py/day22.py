import re
from collections import deque
from enum import Enum
from pathlib import Path
from typing import NamedTuple

input_dir = Path(__file__).parent.parent / 'inputs'

SPACE = '.'
WALL = '#'
LEFT = (0, -1)
RIGHT = (0, 1)
UP = (-1, 0)
DOWN = (1, 0)


class Pos(NamedTuple):
    r: int
    c: int

    def adjacent(self):
        yield Pos(self.r + 1, self.c)
        yield Pos(self.r - 1, self.c)
        yield Pos(self.r, self.c + 1)
        yield Pos(self.r, self.c - 1)

    def go(self, other):
        assert isinstance(other, Direction)
        dr, dc = other.value[0]
        return Pos(self.r+dr, self.c+dc)


class Direction(Enum):
    RIGHT = (0, 1), 0
    DOWN = (1, 0), 1
    LEFT = (0, -1), 2
    UP = (-1, 0), 3

    @property
    def point_value(self):
        return self.value[-1]

    def turn(self, lr):
        match lr:
            case 'R':
                new_point_value = (self.point_value + 1) % 4
            case 'L':
                new_point_value = (self.point_value - 1) % 4
            case _:
                raise RuntimeError('How unusual')
        return {
            d for d in Direction
            if d.point_value == new_point_value
        }.pop()

    def __add__(self, other):
        pass

    def __radd__(self, other):
        (dr, dc), _ = self.value
        return other.__class__(other.r + dr, other.c + dc)

    def __str__(self):
        return FACING[self.value[0]]

    def __repr__(self):
        return str(self)


FACING = {
    RIGHT: '>',
    LEFT: '<',
    DOWN: 'v',
    UP: '^',
}


def parse_input(filename):
    with open(filename) as f:
        grid, _, path = f.read().partition('\n\n')
    # Make grid sparse
    board = {
        Pos(r, c): ch
        for r, row in enumerate(grid.splitlines())
        for c, ch in enumerate(row)
        if ch != ' '
    }
    start = min(k for k, v in board.items() if v == '.')
    steps = [
        match if match in 'LR' else int(match)
        for match in re.findall(r'\d+|[LR]', path)
    ]
    return board, start, steps


def part1(filename):
    def get_wraps(board):
        lefts = sorted([
            pos
            for pos in board
            if pos + Direction.LEFT not in board
        ])
        rights = sorted([
            pos
            for pos in board
            if pos + Direction.RIGHT not in board
        ])
        tops = sorted([
            pos
            for pos in board
            if pos + Direction.UP not in board
        ], key=lambda x: x[1])
        bottoms = sorted([
            pos
            for pos in board
            if pos + Direction.DOWN not in board
        ], key=lambda x: x[1])

        left_map = {
            (left_pos, Direction.LEFT): (right_pos, Direction.LEFT)
            for left_pos, right_pos in zip(lefts, rights)
        }
        right_map = {
            (right_pos, Direction.RIGHT): (left_pos, Direction.RIGHT)
            for left_pos, right_pos in zip(lefts, rights)
        }
        top_map = {
            (top_pos, Direction.UP): (bottom_pos, Direction.UP)
            for top_pos, bottom_pos in zip(tops, bottoms)
        }
        bottom_map = {
            (bottom_pos, Direction.DOWN): (top_pos, Direction.DOWN)
            for top_pos, bottom_pos in zip(tops, bottoms)
        }
        return {**left_map, **right_map, **top_map, **bottom_map}

    return solve(filename, get_wraps)


def part2(filename):
    def get_wraps(board):
        pass

    return solve(filename, get_wraps)


def solve(filename, wrap_func):
    board, pos, steps = parse_input(filename)
    wrap_map = wrap_func(board)
    d = Direction.RIGHT

    def try_move(num_steps):
        nonlocal board, pos, d
        for _ in range(num_steps):
            new_pos, new_dir = wrap_map.get(
                (pos, d),
                (pos + d, d)
            )
            if board[new_pos] == WALL:
                return
            pos, d = new_pos, new_dir

    for step in steps:
        if isinstance(step, int):
            try_move(step)
        else:
            d = d.turn(step)

    return 1000 * (pos.r + 1) + 4 * (pos.c + 1) + d.point_value


if __name__ == '__main__':
    assert part1(input_dir / 'sample22.txt') == 6032
    print('Part 1:', part1(input_dir / 'day22.txt'))
    assert part2(input_dir / 'sample22.txt') == 5031
    print('Part 2:', part2(input_dir / 'day22.txt'))
