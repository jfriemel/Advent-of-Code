import os


def part_a(puzzle_input: str) -> str:
    signal = puzzle_input.strip()
    last_four = [signal[0], signal[1], signal[2], ' ']
    for i in range(3, len(signal)):
        last_four[i % 4] = signal[i]
        if len(last_four) == len(set(last_four)):
            return str(i + 1)
    return 'invalid'


def part_b(puzzle_input: str) -> str:
    signal = puzzle_input.strip()
    last_four = []
    for i in range(13):
        last_four.append(signal[i])
    last_four.append(' ')
    for i in range(13, len(signal)):
        last_four[i % 14] = signal[i]
        if len(last_four) == len(set(last_four)):
            return str(i + 1)
    return 'invalid'


if __name__ == '__main__':
    input_str = open(os.path.join(os.path.dirname(__file__), '..', 'input', 'input06.txt')).read()
    print(f'Part A: {part_a(input_str)}')
    print(f'Part B: {part_b(input_str)}')
