from math import log, ceil
from pathlib import Path

import pytest

input_dir = Path(__file__).parent.parent / 'inputs'

digits = {
    '2': 2,
    '1': 1,
    '0': 0,
    '-': -1,
    '=': -2,
}


def snafu2dec(num):
    return sum(
        5 ** i * digits[d]
        for i, d in enumerate(reversed(num))
    )


def dec2snafu(num):
    mapping = {v: k for k, v in digits.items()}
    value = ''
    while num:
        digit = (num + 2) % 5 - 2
        num = (num - digit) // 5
        value = f'{mapping[digit]}{value}'
    return value


@pytest.mark.parametrize('value, expected', [
    ('1=-0-2', 1747),
    ('12111', 906),
    ('2=0=', 198),
    ('21', 11),
    ('2=01', 201),
    ('111', 31),
    ('20012', 1257),
    ('112', 32),
    ('1=-1=', 353),
    ('1-12', 107),
    ('12', 7),
    ('1=', 3),
    ('122', 37),
])
def test_snafu2dec(value, expected):
    assert snafu2dec(value) == expected


@pytest.mark.parametrize('value, expected', [
    (1, '1'),
    (2, '2'),
    (3, '1='),
    (4, '1-'),
    (5, '10'),
    (6, '11'),
    (7, '12'),
    (8, '2='),
    (9, '2-'),
    (10, '20'),
    (15, '1=0'),
    (20, '1-0'),
    (2022, '1=11-2'),
    (12345, '1-0---0'),
    (314159265, '1121-1110-1=0'),
])
def test_dec2snafu(value, expected):
    assert dec2snafu(value) == expected


def part1(filename):
    with open(filename) as f:
        return dec2snafu(sum(snafu2dec(n.strip()) for n in f))


if __name__ == '__main__':
    print(part1(input_dir / 'day25.txt'))
