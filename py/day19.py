from collections import deque
from concurrent.futures import ProcessPoolExecutor
from enum import Enum, auto
from functools import reduce
from math import ceil
from pathlib import Path

input_dir = Path(__file__).parent.parent / 'inputs'


class Resource(Enum):
    GEODE = auto()
    OBSIDIAN = auto()
    CLAY = auto()
    ORE = auto()


class Blueprint:
    def __init__(self,
                 id,
                 ore_bot_cost,
                 clay_bot_cost,
                 obsidian_bot_ore_cost,
                 obsidian_bot_clay_cost,
                 geode_bot_ore_cost,
                 geode_bot_obsidian_cost):
        self.id = id
        self.costs = {
            Resource.GEODE: {Resource.ORE: geode_bot_ore_cost,
                             Resource.OBSIDIAN: geode_bot_obsidian_cost},
            Resource.OBSIDIAN: {Resource.ORE: obsidian_bot_ore_cost,
                                Resource.CLAY: obsidian_bot_clay_cost},
            Resource.CLAY: {Resource.ORE: clay_bot_cost},
            Resource.ORE: {Resource.ORE: ore_bot_cost},
        }

    @classmethod
    def from_line(cls, line):
        nums = [int(word) for word in line.replace(':', '').split() if word.isnumeric()]
        return cls(*nums)

    def quality(self, time):
        return self.id * self.max_geodes(time)

    def _potential(self, remaining_time, robots, resources):
        added_bots = {r: 0 for r in Resource}
        resources = resources.copy()
        for _ in range(remaining_time):
            resources = {
                r: resources[r] + robots[r] + added_bots[r]
                for r in Resource
            }
            for robot_type in Resource:
                if all(
                        resources[r] >= n * (added_bots[robot_type] + 1)
                        for r, n in self.costs[robot_type].items()
                ):
                    added_bots[robot_type] += 1
        return resources[Resource.GEODE]

    def max_geodes(self, allowed_time):
        best = 0
        # State: time remaining, current robots, current resources
        init_state = allowed_time, {res: 0 for res in Resource}, {res: 0 for res in Resource}
        init_state[1][Resource.ORE] = 1
        q = deque([init_state])
        while q:
            t, robots, resources = q.popleft()
            if self._potential(t, robots, resources) < best:
                continue
            forecast = resources[Resource.GEODE] + robots[Resource.GEODE] * t
            if forecast > best:
                best = forecast
            for bot_type, cost in self.costs.items():
                if any(robots[res] == 0 for res in cost):
                    continue
                needed = {
                    res: n - resources[res]
                    for res, n in cost.items()
                }
                required_time = max(
                    ceil(n / robots[res]) if n > 0 else 0
                    for res, n in needed.items()
                ) + 1
                if required_time > t:
                    continue
                new_resources = {
                    r: resources[r] + required_time * robots[r] - self.costs[bot_type].get(r, 0)
                    for r in Resource
                }
                new_bots = robots.copy()
                new_bots[bot_type] += 1
                state = t - required_time, new_bots, new_resources
                if bot_type in (Resource.OBSIDIAN, Resource.GEODE):
                    q.appendleft(state)
                else:
                    q.append(state)
        return best


def parse_input(filename):
    with open(filename) as f:
        lines = f.readlines()
    return [Blueprint.from_line(line) for line in lines]


def part1_worker(blueprint):
    return blueprint.quality(24)


def part1(filename):
    blueprints = parse_input(filename)
    with ProcessPoolExecutor() as executor:
        return sum(executor.map(part1_worker, blueprints))


def part2_worker(blueprint):
    return blueprint.max_geodes(32)


def part2(filename):
    blueprints = parse_input(filename)[:3]
    with ProcessPoolExecutor() as executor:
        nums = executor.map(part2_worker, blueprints)
    return reduce(int.__mul__, nums)


if __name__ == '__main__':
    assert part1(input_dir / 'sample19.txt') == 33
    print('Part 1:', part1(input_dir / 'day19.txt'))
    assert part2(input_dir / 'sample19.txt') == 56*62
    print('Part 2:', part2(input_dir / 'day19.txt'))