from enum import Enum
from itertools import cycle
from math import gcd
from pathlib import Path
from typing import NamedTuple, Set

input_dir = Path(__file__).parent.parent / 'inputs'


class Point(NamedTuple):
    x: int
    y: int

    def nudge(self, d):
        """
        :param Dir d:
        :return: New position
        :rtype: Point
        """
        return self + d.value

    def __add__(self, other):
        return Point(self.x + other.x, self.y + other.y)

    def __mul__(self, factor):
        return Point(self.x * factor, self.y * factor)


class Dir(Point, Enum):
    UP = Point(0, 1)
    DOWN = Point(0, -1)
    LEFT = Point(-1, 0)
    RIGHT = Point(1, 0)


class Shape(Enum):
    H_LINE = {Point(2, 0), Point(3, 0), Point(4, 0), Point(5, 0)}
    PLUS = {Point(3, 0), Point(2, 1), Point(3, 1), Point(4, 1), Point(3, 2)}
    L = {Point(2, 0), Point(3, 0), Point(4, 0), Point(4, 1), Point(4, 2)}
    V_LINE = {Point(2, 0), Point(2, 1), Point(2, 2), Point(2, 3)}
    SQUARE = {Point(2, 0), Point(3, 0), Point(2, 1), Point(3, 1)}


class FallingPiece:
    """
    :type points: set[Point]
    """

    def __init__(self, shape: Shape, start_height: int):
        self.points = {p + Dir.UP * start_height for p in shape.value}

    def move(self, d: Dir):
        self.points = {p.nudge(d) for p in self.points}

    def try_move(self, d: Dir, chamber: Set[Point]):
        new_pos = {p.nudge(d) for p in self.points}
        if min(p.x for p in new_pos) < 0 or max(p.x for p in new_pos) > 6:
            return False
        if min(p.y for p in new_pos) < 0:
            return False
        if new_pos & chamber:
            return False
        self.points = new_pos
        return True


NO_ROCK = FallingPiece(Shape.PLUS, 0)
NO_ROCK.points = set()

# For rendering/debugging
EMPTY = '.'
BLOCK = '#'
FALLING = '@'


# BLOCK = chr(0x2588)
# FALLING = chr(0x2592)


def parse_input(filename):
    mapping = {'<': Dir.LEFT, '>': Dir.RIGHT}
    with open(filename) as f:
        return [mapping[ch] for ch in f.read().strip()]


def draw(cavern: Set[Point], falling_rock: FallingPiece, max_rows: int = 20):
    LABELS = True
    points = cavern | falling_rock.points
    ymax = max(p.y for p in points)
    ymin = max(min(p.y for p in points), ymax - max_rows)
    if not cavern:
        ymin = 0
    width = len(str(ymax)) + 1
    for y in range(ymax, ymin - 1, -1):
        line = f'{y:<{width}} |' if LABELS else '|'
        for x in range(7):
            if (x, y) in falling_rock.points:
                line += FALLING
            elif (x, y) in cavern:
                line += BLOCK
            else:
                line += EMPTY
        line += '|'
        print(line)
    if ymin == 0:
        if LABELS:
            print(' ' * (width + 1), end='')
        print('+-------+')
    print()


def drop_shape(gas_seq, shape, cavern):
    """
    Drop a shape into the given cavern using the gas and rocks generators
    """
    if not cavern:
        starting_height = 3
    else:
        starting_height = max(p.y for p in cavern or [Point(0, -1)]) + 4
    rock = FallingPiece(shape, starting_height)
    for i, push in gas_seq:
        rock.try_move(push, cavern)
        if not rock.try_move(Dir.DOWN, cavern):
            return i, rock


def part1(filename):
    gas = cycle(enumerate(parse_input(filename)))
    rocks = cycle(Shape)
    cavern = set()
    for _ in range(2022):
        _, settled = drop_shape(gas, next(rocks), cavern)
        cavern |= settled.points
    return max(p.y for p in cavern) + 1


def part2(filename):
    max_pieces = 1000000000000
    gas = parse_input(filename)
    gas = cycle(enumerate(gas))
    rocks = cycle(enumerate(Shape))
    cavern = set()
    history = []
    heights = []
    while True:
        r_index, rock = next(rocks)
        i, settled = drop_shape(gas, rock, cavern)
        cavern |= settled.points
        heights.append(max(p.y for p in cavern) + 1)
        key = r_index, i
        history.append(key)
        if history.count(key) > 3:
            break
    # Check cycle length
    cycle_length = [i for i, x in enumerate(reversed(history)) if x == history[-1]][1]
    height_per_cycle = heights[-1] - heights[-1 - cycle_length]
    mod = max_pieces % cycle_length
    piece_number, height = max((i, h) for i, h in enumerate(heights, start=1) if i % cycle_length == mod)
    cycles = (max_pieces - piece_number) // cycle_length
    return height + height_per_cycle * cycles


if __name__ == '__main__':
    assert part1(input_dir / 'sample17.txt') == 3068
    print('Part 1:', part1(input_dir / 'day17.txt'))
    assert part2(input_dir / 'sample17.txt') == 1514285714288
    print('Part 2:', part2(input_dir / 'day17.txt'))
