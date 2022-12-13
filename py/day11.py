from pathlib import Path
import logging
from typing import Dict

logging.basicConfig(format='%(message)s')
logging.root.setLevel(logging.INFO)


class Monkey:

    def __init__(self, id, items, inspection, test, true_target, false_target):
        """
        :type id: int
        :type items: list[int]
        :type inspection: typing.Callable[[int], int]
        :type test: typing.Callable[[int], bool]
        :type true_target: int
        :type false_target: int
        """
        self.id = id
        self.items = items
        self.inspection = inspection
        self.test = test
        self.true_target = true_target
        self.false_target = false_target
        self.count = 0

    def __repr__(self):
        items = ', '.join(map(str, self.items)) or 'no items'
        return f'<Monkey {self.id}: {items}, throws to {self.true_target} or {self.false_target}>'

    def __str__(self):
        return f'Monkey {self.id}: {", ".join(map(str, self.items))}'

    @staticmethod
    def from_text(text):
        lines = text.splitlines()
        id_number = int(lines[0].split()[-1][:-1])
        items = [int(item) for item in lines[1].partition(': ')[-1].split(', ')]
        *_, operator, operand = lines[2].split()
        assert operator in '+*'
        assert operand == 'old' or operand.isdigit()
        func = eval(f'lambda old: (old {operator} {operand}) // 3')
        test_value = int(lines[3].split()[-1])
        true = int(lines[4].split()[-1])
        false = int(lines[5].split()[-1])

        return Monkey(
            id=id_number,
            items=items,
            inspection=func,
            test=lambda x: x % test_value == 0,
            true_target=true,
            false_target=false,
        )

    def inspect_items(self, other_monkeys):
        """

        :type other_monkeys: dict[int, Monkey]
        :return:
        """
        logging.debug(f'Monkey {self.id}:')
        for item in self.items:
            new_value = self.inspection(item)
            if self.test(new_value):
                target = self.true_target
            else:
                target = self.false_target
            logging.debug(f'Passing item with worry level {new_value} to monkey {target}')
            other_monkeys[target].items.append(new_value)
        self.count += len(self.items)
        self.items.clear()


def parse_input(filename: str) -> Dict[int, Monkey]:
    with open(filename) as f:
        return {(m := Monkey.from_text(t)).id: m for t in f.read().split('\n\n')}


def do_round(monkeys: Dict[int, Monkey]):
    for i, m in sorted(monkeys.items()):
        m.inspect_items(monkeys)
    for m in monkeys.values():
        logging.debug(m)


def solve(filename):
    monkeys = parse_input(filename)
    for i, m in monkeys.items():
        logging.debug(m)
    for _ in range(20):
        do_round(monkeys)
    throw_totals = sorted([m.count for m in monkeys.values()], reverse=True)
    return throw_totals[0] * throw_totals[1]


if __name__ == '__main__':
    sample_file = Path(__file__).parent.parent / 'inputs' / 'sample11.txt'
    input_file = Path(__file__).parent.parent / 'inputs' / 'day11.txt'

    assert solve(sample_file) == 10605
    print(solve(input_file))
