from pathlib import Path

input_dir = Path(__file__).parent.parent / 'inputs'


def parse_input(filename):
    with open(filename) as f:
        lines = f.read().splitlines()
    width = len(lines[0])
    height = len(lines)
    start = 0, lines[0].index('.')
    goal = height - 1, lines[-1].index('.')
    blizzards = {
        (ch, r, c)
        for r, row in enumerate(lines)
        for c, ch in enumerate(row)
        if ch in '<>^v'
    }
    return blizzards, (height, width), start, goal


def solve(filename):
    blizzards, (height, width), start, goal = parse_input(filename)
    walls = (
                    {(r, 0) for r in range(height)} |
                    {(r, width - 1) for r in range(height)} |
                    {(0, c) for c in range(width)} |
                    {(height - 1, c) for c in range(width)}
            ) - {start, goal}

    def move_blizzard(char, row, column):
        nonlocal height, width
        match char:
            case '<':
                new_pos = row, column - 1 or width - 2
            case '>':
                new_pos = row, 1 if column == width - 2 else column + 1
            case '^':
                new_pos = row - 1 or height - 2, column
            case 'v':
                new_pos = 1 if row == height - 2 else row + 1, column
            case _:
                raise ValueError(f'Invalid blizzard: {char}')
        return char, *new_pos

    def print_valley(pos):
        nonlocal blizzards, height, width, start, goal
        for r in range(height):
            for c in range(width):
                if (r, c) in pos:
                    print('E', end='')
                elif r in (0, height - 1) or c in (0, width - 1):
                    if (r, c) in [start, goal]:
                        print('.', end='')
                    else:
                        print('#', end='')
                else:
                    blizz_here = [ch for (ch, x, y) in blizzards if (x, y) == (r, c)]
                    if len(blizz_here) == 0:
                        print('.', end='')
                    elif len(blizz_here) == 1:
                        print(blizz_here.pop(), end='')
                    else:
                        print(len(blizz_here), end='')
            print()

    t = 0

    def search(init, target):
        nonlocal blizzards, t
        pos = {init}
        while target not in pos:
            # First, the blizzards move
            blizzards = {move_blizzard(*b) for b in blizzards}
            blizz_pos = {(r, c) for _, r, c in blizzards}
            t += 1
            # Determine which places can be reached at this t
            adjacent = (
                    pos |
                    {(r, c + 1) for r, c in pos} |
                    {(r, c - 1) for r, c in pos} |
                    {(r + 1, c) for r, c in pos} |
                    {(r - 1, c) for r, c in pos}
            )
            # Filter out invalid values and make those the new possible positions
            pos = {
                p for r, c in adjacent
                if r >= 0 and (p := (r, c)) not in walls and p not in blizz_pos
            }
    search(start, goal)
    yield t
    search(goal, start)
    search(start, goal)
    yield t


if __name__ == '__main__':
    assert list(solve(input_dir / 'sample24.txt')) == [18, 54]
    s = solve(input_dir / 'day24.txt')
    print('Part 1:', next(s))
    print('Part 2:', next(s))
