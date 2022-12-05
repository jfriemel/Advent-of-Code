import collections
import os
import re


def part_a(puzzle_input: str) -> str:
    input_parts = list(map(lambda l: l.split('\n'), puzzle_input.rstrip().split('\n\n')))

    # Create input stacks
    stacks = _get_input_stacks(input_parts[0])

    # Move items in stacks
    for command in input_parts[1]:
        instr = re.findall(r'\d+', command)
        for i in range(int(instr[0])):
            item = stacks[int(instr[1]) - 1].pop()
            stacks[int(instr[2]) - 1].append(item)

    # Collect top items of stacks
    result = ''
    for i in range(len(stacks)):
        result += stacks[i].pop()
    return result


def part_b(puzzle_input: str) -> str:
    input_parts = list(map(lambda l: l.split('\n'), puzzle_input.rstrip().split('\n\n')))

    # Create input stacks
    stacks = _get_input_stacks(input_parts[0])

    # Move items in stacks
    for command in input_parts[1]:
        instr = re.findall(r'\d+', command)
        items = collections.deque()
        for i in range(int(instr[0])):
            items.appendleft(stacks[int(instr[1]) - 1].pop())
        for item in items:
            stacks[int(instr[2]) - 1].append(item)

    # Collect top items of stacks
    result = ''
    for i in range(len(stacks)):
        result += stacks[i].pop()
    return result


def _get_input_stacks(stack_descr: str) -> dict[int, collections.deque]:
    num_stacks = int(re.findall(r'\d+', stack_descr[-1])[-1])
    stacks = {}
    for i in range(num_stacks):
        stacks[i] = collections.deque()
        for j in range(len(stack_descr) - 2, -1, -1):
            if len(stack_descr[j]) > 4 * i + 1 and stack_descr[j][4 * i + 1] != ' ':
                stacks[i].append(stack_descr[j][4 * i + 1])
    return stacks


if __name__ == '__main__':
    input_str = open(os.path.join(os.path.dirname(__file__), '..', 'input', 'input05.txt')).read()
    print(f'Part A: {part_a(input_str)}')
    print(f'Part B: {part_b(input_str)}')
