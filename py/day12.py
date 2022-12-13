import string
import sys
from collections import deque, defaultdict
from pathlib import Path

HEIGHTS = {
    'S': 0,
    'E': 25,
    **dict(zip(string.ascii_lowercase, range(26)))
}


def parse_input(filename):
    with open(filename) as f:
        mapping = {
            (r, c): ch
            for r, line in enumerate(f.read().splitlines())
            for c, ch in enumerate(line)
        }
    return mapping


def dests(field, coord):
    max_height = HEIGHTS[field[coord]] + 1
    x, y = coord
    options = [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]
    return [c for c in options if c in field and HEIGHTS[field[c]] <= max_height]


def part1(filename):
    grid = parse_input(filename)
    start = [k for k, v in grid.items() if v == 'S'].pop()
    end = [k for k, v in grid.items() if v == 'E'].pop()
    return solve(grid, start, end)


def part2(filename):
    grid = parse_input(filename)
    starts = [k for k, v in grid.items() if HEIGHTS[v] == HEIGHTS['a']]
    end = [k for k, v in grid.items() if v == 'E'].pop()
    return min(solve(grid, start, end) for start in starts)


def solve(grid, start, end):
    distances = defaultdict(lambda: sys.maxsize, {start: 0})
    q = deque([start])
    while q and end not in distances:
        coord = q.popleft()
        new_dist = distances[coord] + 1
        for dest in dests(grid, coord):
            if distances[dest] < new_dist:
                continue
            distances[dest] = new_dist
            if dest not in q:
                q.append(dest)
    return distances[end]


if __name__ == '__main__':
    sample_file = Path(__file__).parent.parent / 'inputs' / 'sample12.txt'
    input_file = Path(__file__).parent.parent / 'inputs' / 'day12.txt'
    assert part1(sample_file) == 31
    print(part1(input_file))
    assert part2(sample_file) == 29
    print(part2(input_file))
