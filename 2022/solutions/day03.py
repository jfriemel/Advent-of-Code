import os


def part_a(puzzle_input: str) -> str:
    rucksacks = puzzle_input.strip().split('\n')
    sum_priorities = 0
    for rucksack in rucksacks:
        mid = len(rucksack) // 2
        in_both = set(rucksack[:mid]).intersection(set(rucksack[mid:])).pop()
        if in_both.isupper():
            sum_priorities += ord(in_both) - ord('A') + 27
        else:
            sum_priorities += ord(in_both) - ord('a') + 1
    return str(sum_priorities)


def part_b(puzzle_input: str) -> str:
    rucksacks = puzzle_input.strip().split('\n')
    it = iter(rucksacks)
    groups = zip(it, it, it)
    sum_priorities = 0
    for (elf_a, elf_b, elf_c) in groups:
        in_all = set(elf_a).intersection(set(elf_b)).intersection(set(elf_c)).pop()
        if in_all.isupper():
            sum_priorities += ord(in_all) - ord('A') + 27
        else:
            sum_priorities += ord(in_all) - ord('a') + 1
    return str(sum_priorities)


if __name__ == '__main__':
    input_str = open(os.path.join(os.path.dirname(__file__), '..', 'input', 'input03.txt')).read()
    print(f'Part A: {part_a(input_str)}')
    print(f'Part B: {part_b(input_str)}')
