import re
from pathlib import Path

input_dir = Path(__file__).parent.parent / 'inputs'

SPACE = '.'
WALL = '#'
FACING = {
    (0, 1): '>',
    (0, -1): '<',
    (1, 0): 'v',
    (-1, 0): '^',
}


def parse_input(filename):
    with open(filename) as f:
        grid, _, path = f.read().partition('\n\n')
    # Make grid sparse
    board = {
        (r, c): ch
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
    board, (r, c), steps = parse_input(filename)
    dr, dc = 0, 1

    def turn(d):
        nonlocal r, c, dr, dc
        if d == 'R':
            dr, dc = dc, -dr
        elif d == 'L':
            dr, dc = -dc, dr
        board[r, c] = FACING[dr, dc]

    def try_move(num_steps):
        nonlocal board, r, c, dr, dc
        for _ in range(num_steps):
            board[r, c] = FACING[dr, dc]
            new_r, new_c = r + dr, c + dc
            # Handle wrapping
            if (new_r, new_c) not in board:
                # Step backwards until off the board again
                new_r, new_c = r, c
                while (new_r, new_c) in board:
                    new_r -= dr
                    new_c -= dc
                # Get back on the board
                new_r += dr
                new_c += dc
            if board[new_r, new_c] == WALL:
                return
            r, c = new_r, new_c

    for step in steps:
        if isinstance(step, int):
            try_move(step)
        else:
            turn(step)

    facing = {
        (0, 1): 0,
        (1, 0): 1,
        (0, -1): 2,
        (-1, 0): 3,
    }

    # for row in range(max(ri for ri, ci in board) + 1):
    #     for col in range(max(ci for ri, ci in board) + 1):
    #         print(board.get((row, col), ' '), end='')
    #     print()

    print('Final Row:', r+1)
    print('Final Col:', c+1)
    print(f'Facing: {FACING[dr, dc]} ({facing[dr, dc]})')
    return 1000 * (r + 1) + 4 * (c + 1) + facing[dr, dc]


if __name__ == '__main__':
    assert part1(input_dir / 'sample22.txt') == 6032
    print('Part 1:', part1(input_dir / 'day22.txt'))