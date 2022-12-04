import functools
import os


def part_a(puzzle_input: str) -> str:
    elves = puzzle_input.strip().split('\n\n')
    max_elf = 0
    for elf in elves:
        sum_elf = functools.reduce(lambda a, b: a + b, map(int, elf.split('\n')))
        if sum_elf > max_elf:
            max_elf = sum_elf
    return str(max_elf)


def part_b(puzzle_input: str) -> str:
    elves = puzzle_input.strip().split('\n\n')
    elf_sums = []
    for elf in elves:
        elf_sums.append(functools.reduce(lambda a, b: a + b, map(int, elf.split('\n'))))
    return str(sum(sorted(elf_sums, reverse=True)[:3]))


if __name__ == '__main__':
    input_str = open(os.path.join(os.path.dirname(__file__), '..', 'input', 'input01.txt')).read()
    print(f'Part A: {part_a(input_str)}')
    print(f'Part B: {part_b(input_str)}')
