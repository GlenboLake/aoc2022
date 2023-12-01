from collections import Counter
from enum import Enum
from itertools import count
from pathlib import Path
from typing import NamedTuple, Set

input_dir = Path(__file__).parent.parent / 'inputs'

small_sample = '''\
.....
..##.
..#..
.....
..##.
.....'''


class Direction(Enum):
    NORTH = (1, -1), (0, -1), (-1, -1)
    SOUTH = (1, 1), (0, 1), (-1, 1)
    WEST = (-1, 1), (-1, 0), (-1, -1)
    EAST = (1, 1), (1, 0), (1, -1)
    ALL = (1, 1), (1, 0), (1, -1), (0, 1), (0, -1), (-1, 1), (-1, 0), (-1, -1)


class Elf(NamedTuple):
    x: int
    y: int

    def __add__(self, other):
        dx, dy = other
        return Elf(self.x + dx, self.y + dy)

    def neighbors(self, direction=Direction.ALL):
        return [
            Elf(self.x + dx, self.y + dy)
            for dx, dy in direction.value
        ]

    def propose_move(self, elves, priority_order):
        """
        :type elves: Set[Elf]
        :type priority_order: list[Direction]
        :rtype: Elf
        """
        # No neighbors? Do nothing
        if not elves.intersection(self.neighbors()):
            return self
        for direction in priority_order:
            if not elves.intersection(self.neighbors(direction)):
                dx, dy = direction.value[1]
                return Elf(self.x + dx, self.y + dy)
        return self


def parse_input(filename):
    with open(filename) as f:
        data = f.read().splitlines()
    elves = {
        Elf(x, y)
        for y, line in enumerate(data)
        for x, ch in enumerate(line)
        if ch == '#'
    }
    return elves


def propose_moves(elves, directions):
    return {
        elf: elf.propose_move(elves, directions)
        for elf in elves
    }


def part1(filename):
    elves = parse_input(filename)

    directions = [Direction.NORTH, Direction.SOUTH, Direction.WEST, Direction.EAST]
    for i in range(1, 11):
        moves = propose_moves(elves, directions)
        allowed_moves = {
            k
            for k, v in Counter(moves.values()).items()
            if v == 1
        }
        elves = {
            after if after in allowed_moves else before
            for before, after in moves.items()
        }
        directions = [*directions[1:], directions[0]]
    xmin = min(e.x for e in elves)
    xmax = max(e.x for e in elves)
    ymin = min(e.y for e in elves)
    ymax = max(e.y for e in elves)
    return (ymax - ymin + 1) * (xmax - xmin + 1) - len(elves)


def part2(filename):
    elves = parse_input(filename)
    directions = [Direction.NORTH, Direction.SOUTH, Direction.WEST, Direction.EAST]

    for i in count(1):
        moves = propose_moves(elves, directions)
        if all(k == v for k, v in moves.items()):
            return i
        allowed_moves = {
            k
            for k, v in Counter(moves.values()).items()
            if v == 1
        }
        elves = {
            after if after in allowed_moves else before
            for before, after in moves.items()
        }
        directions = [*directions[1:], directions[0]]


if __name__ == '__main__':
    assert part1(input_dir / 'sample23.txt') == 110
    print('Part 1:', part1(input_dir / 'day23.txt'))
    assert part2(input_dir / 'sample23.txt') == 20
    print('Part 2:', part2(input_dir / 'day23.txt'))
