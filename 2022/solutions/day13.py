import os
from functools import cmp_to_key


def compare(left: list | int, right: list | int) -> int:
    match left, right:
        case int(), int():
            return left - right
        case int(), list():
            return compare([left], right)
        case list(), int():
            return compare(left, [right])
        case list(), list():
            for result in map(compare, left, right):
                if result != 0:
                    return result
            return len(left) - len(right)


def part_a(puzzle_input: str) -> str:
    pairs = list(map(lambda s: tuple(map(eval, s.split('\n'))), puzzle_input.strip().split('\n\n')))
    result = 0
    for idx, (left, right) in enumerate(pairs, 1):
        if compare(left, right) < 0:
            result += idx
    return str(result)


def part_b(puzzle_input: str) -> str:
    packets = list(map(eval, filter(lambda s: len(s) > 0, puzzle_input.strip().split('\n'))))
    packets.append([[2]])
    packets.append([[6]])
    packets = sorted(packets, key=cmp_to_key(compare))  # sort using custom compare function
    return str((packets.index([[2]]) + 1) * (packets.index([[6]]) + 1))


if __name__ == '__main__':
    input_str = open(os.path.join(os.path.dirname(__file__), '..', 'input', 'input13.txt')).read()
    print(f'Part A: {part_a(input_str)}')
    print(f'Part B: {part_b(input_str)}')
