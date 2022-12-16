import os
import re


def part_a(puzzle_input: str, excl_row: int = 2_000_000) -> str:
    pos_pairs = map(lambda l: tuple(map(int, re.findall(r'-?\d+', l))), puzzle_input.strip().split('\n'))
    exclusions = set()
    inclusions = set()
    for sx, sy, bx, by in pos_pairs:
        if by == excl_row:
            inclusions.add(bx)  # do not count positions that are occupied by a beacon
        dist = (abs(sx - bx) + abs(sy - by)) - abs(sy - excl_row)
        for i in range(dist + 1):
            exclusions.add(sx + i)
            exclusions.add(sx - i)
    return str(len(exclusions - inclusions))


def part_b(puzzle_input: str, max_pos: int = 4_000_000) -> str:
    pos_pairs = map(lambda l: tuple(map(int, re.findall(r'-?\d+', l))), puzzle_input.strip().split('\n'))
    sensors = []
    for sx, sy, bx, by in pos_pairs:
        sensors.append((sx, sy, abs(sx - bx) + abs(sy - by)))

    for sx, sy, s_dist in sensors:
        for i in range(s_dist + 2):
            # Only check positions that are exactly one step further away from the sensor than the sensor's beacon.
            # The distress beacon would not be unique if it was further away than one step from all sensors.
            for cx, cy in [(sx + i, sy + s_dist + 1 - i), (sx - i, sy + s_dist + 1 - i), (sx + i, sy - s_dist - 1 + i),
                           (sx - i, sy - s_dist - 1 + i)]:
                if not (0 <= cx <= max_pos and 0 <= cy <= max_pos):  # ensure that candidate is within search space
                    continue
                for tx, ty, t_dist in sensors:  # check that candidate is out of range of all sensors
                    if abs(tx - cx) + abs(ty - cy) <= t_dist:
                        if cx == 14 and cy == 11:
                            print(tx, ty, t_dist)
                        break
                else:
                    return str(cx * 4_000_000 + cy)

    return 'Invalid input, no beacon candidate found.'


if __name__ == '__main__':
    input_str = open(os.path.join(os.path.dirname(__file__), '..', 'input', 'input15.txt')).read()
    print(f'Part A: {part_a(input_str)}')
    print(f'Part B: {part_b(input_str)}')
