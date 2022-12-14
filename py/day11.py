import functools
from pathlib import Path
from typing import Dict

DEBUG = False
if DEBUG:
    def debug(*args, **kwargs):
        print(*args, **kwargs)
else:
    def debug(*args, **kwargs):
        pass


class Monkey:

    def __init__(self, id, items, inspection, test_value, true_target, false_target):
        """
        :type id: int
        :type items: list[int]
        :type inspection: typing.Callable[[int], int]
        :type test_value: int
        :type true_target: int
        :type false_target: int
        """
        self.id = id
        self.items = items
        self.inspection = inspection
        self.test_value = test_value
        self.true_target = true_target
        self.false_target = false_target
        self.count = 0

    def __repr__(self):
        items = ', '.join(map(str, self.items)) or 'no items'
        return f'<Monkey {self.id}: {items}, throws to {self.true_target} or {self.false_target}>'

    def __str__(self):
        return f'Monkey {self.id}: {", ".join(map(str, self.items))}'

    def report(self):
        debug(f'Monkey {self.id} inspected items {self.count} times.')

    @staticmethod
    def from_text(text):
        lines = text.splitlines()
        id_number = int(lines[0].split()[-1][:-1])
        items = [int(item) for item in lines[1].partition(': ')[-1].split(', ')]
        *_, operator, operand = lines[2].split()
        assert operator in '+*'
        assert operand == 'old' or operand.isdigit()
        func = eval(f'lambda old: (old {operator} {operand})')
        test_value = int(lines[3].split()[-1])
        true = int(lines[4].split()[-1])
        false = int(lines[5].split()[-1])

        return Monkey(
            id=id_number,
            items=items,
            inspection=func,
            test_value=test_value,
            true_target=true,
            false_target=false,
        )

    def inspect_items(self, other_monkeys, reduce_worry):
        """
        :type other_monkeys: dict[int, Monkey]
        :type reduce_worry: bool
        """
        mod = functools.reduce(lambda a, b: a * b, [m.test_value for m in other_monkeys.values()])
        for item in self.items:
            new_value = self.inspection(item)
            if reduce_worry:
                new_value //= 3
            new_value %= mod
            if new_value % self.test_value == 0:
                target = self.true_target
            else:
                target = self.false_target
            other_monkeys[target].items.append(new_value)
        self.count += len(self.items)
        self.items.clear()


def parse_input(filename: str) -> Dict[int, Monkey]:
    with open(filename) as f:
        return {(m := Monkey.from_text(t)).id: m for t in f.read().split('\n\n')}


def do_round(monkeys: Dict[int, Monkey], reduce_worry: bool):
    for i, m in sorted(monkeys.items()):
        m.inspect_items(monkeys, reduce_worry)


def part1(filename):
    return solve(filename, 20, reduce_worry=True)


def part2(filename):
    return solve(filename, 10_000, reduce_worry=False, debug_at=[1, 20, 1000, 2000, 3000, 4000, 5000, 6000, 7000, 8000, 9000, 10_000])


def solve(filename, max_rounds: int, reduce_worry, debug_at=None):
    if debug_at is None:
        debug_at = []
    monkeys = parse_input(filename)
    for i in range(max_rounds):
        do_round(monkeys, reduce_worry)
        if i + 1 in debug_at:
            debug(f'== After round {i + 1} ==')
            for m in monkeys.values():
                m.report()
    throw_totals = sorted([m.count for m in monkeys.values()], reverse=True)
    return throw_totals[0] * throw_totals[1]


if __name__ == '__main__':
    sample_file = Path(__file__).parent.parent / 'inputs' / 'sample11.txt'
    input_file = Path(__file__).parent.parent / 'inputs' / 'day11.txt'

    assert part1(sample_file) == 10605
    print('Part 1:', part1(input_file))
    assert part2(sample_file) == 2713310158
    print('Part 2:', part2(input_file))