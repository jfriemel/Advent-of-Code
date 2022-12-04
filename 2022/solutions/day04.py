import os


def part_a(puzzle_input: str) -> str:
    assignments = puzzle_input.strip().split('\n')
    num_containments = 0
    for assignment in assignments:
        ranges = assignment.split(',')
        range_1 = list(map(int, ranges[0].split('-')))
        range_2 = list(map(int, ranges[1].split('-')))
        if (range_1[0] <= range_2[0] and range_1[1] >= range_2[1]) or \
                (range_1[0] >= range_2[0] and range_1[1] <= range_2[1]):
            num_containments += 1
    return str(num_containments)


def part_b(puzzle_input: str) -> str:
    assignments = puzzle_input.strip().split('\n')
    num_overlaps = 0
    for assignment in assignments:
        ranges = assignment.split(',')
        range_1 = list(map(int, ranges[0].split('-')))
        range_2 = list(map(int, ranges[1].split('-')))
        if range_1[0] <= range_2[1] and range_1[1] >= range_2[0]:
            num_overlaps += 1
    return str(num_overlaps)


if __name__ == '__main__':
    input_str = open(os.path.join(os.path.dirname(__file__), '..', 'input', 'input04.txt')).read()
    print(f'Part A: {part_a(input_str)}')
    print(f'Part B: {part_b(input_str)}')
