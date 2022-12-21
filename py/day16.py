import re
import sys
from collections import deque
from pathlib import Path
from pprint import pprint
from typing import NamedTuple, List, Dict, Tuple

label_regex = re.compile(r"(?<=Valve )\w+")
flow_regex = re.compile(r"(?<=flow rate=)\d+")
tunnels_regex = re.compile(r"\w+(, \w+)*$")

input_dir = Path(__file__).parent.parent / 'inputs'


class Valve(NamedTuple):
    label: str
    flow_rate: int
    connections: List[str]

    def __repr__(self):
        return f'Valve {self.label} ({self.flow_rate}) -> ' + ','.join(self.connections)


ValveDict = Dict[str, Valve]
DistanceMap = Dict[str, Dict[str, int]]


def parse_input(filename) -> Tuple[ValveDict, DistanceMap]:
    def make_valve(line):
        label = label_regex.search(line).group()
        flow_rate = int(flow_regex.search(line).group())
        tunnels = tunnels_regex.search(line).group().split(', ')
        return Valve(label, flow_rate, tunnels)

    with open(filename) as f:
        valves = {(v := make_valve(line)).label: v for line in f}

    # Not all valves are significant. Those with a flow_rate of 0 don't matter,
    # but we should still keep AA (because it's the starting point) and figure
    # out the distances between all significant valves
    def travel(v):
        nonlocal valves
        distances = {x: 1 for x in valves[v].connections}
        q = deque(distances.items())
        while q:
            other, dist = q.popleft()
            new_dist = dist + 1
            for new_dest in valves[other].connections:
                if new_dest == v or distances.get(new_dest, sys.maxsize) <= new_dist:
                    continue
                distances[new_dest] = new_dist
                q.append((new_dest, new_dist))
        return {label: dist for label, dist in distances.items() if valves[label].flow_rate != 0}

    return valves, {
        k: travel(k)
        for k, v in valves.items() if k == 'AA' or v.flow_rate
    }


def part1(filename):
    valves, distances = parse_input(filename)
    # A state is: t, open valves, and location. Score is current released pressure
    scores = {}  # {(t,open_valves,location): score}
    start = 'AA'
    init_state = (0, frozenset(), start, 0)
    states = deque()
    states.append(init_state)
    while states:
        t, open_valves, location, total_pressure = states.popleft()
        state = t, open_valves, location
        if scores.get(state, -1) >= total_pressure:
            continue
        scores[state] = total_pressure
        if t >= 30:
            continue
        pressure_per_tick = sum(valves[v].flow_rate for v in open_valves)
        if location not in open_valves and location != start:
            states.append((t + 1, open_valves | {location}, location, total_pressure + pressure_per_tick))
        closed_valves = {node: dist for node, dist in distances[location].items() if node not in open_valves}
        if not closed_valves:
            states.append((t + 1, open_valves, location, total_pressure + pressure_per_tick))
        else:
            for node, dist in closed_valves.items():
                new_state = t + dist, open_valves, node, total_pressure + dist * pressure_per_tick
                states.append(new_state)
    return max(score for (t, *_), score in scores.items() if t == 30)


def part2(filename):
    # Same as part 1, but now there are two entities that can move and open valves
    valves, distances = parse_input(filename)
    # A state is now: t, open valves, locations. Score is still current released pressure
    scores = {}  # {(t, open_valves, locations): score}
    start = 'AA'
    init_state = (0, frozenset(), (start, start), 0)
    states = deque()
    states.append(init_state)
    while states:
        t, open_valves, me, elephant, total_pressure = states.popleft()
        state = t, open_valves, me, elephant
        if scores.get(state, -1) >= total_pressure:
            continue
        scores[state] = total_pressure
        if t >= 26:
            continue
        pressure_per_tick = sum(valves[v].flow_rate for v in open_valves)


if __name__ == '__main__':
    assert part1(input_dir / 'sample16.txt') == 1651
    # print(part1(input_dir / 'day16.txt'))
    assert part2(input_dir / 'sample16.txt') == 1707
