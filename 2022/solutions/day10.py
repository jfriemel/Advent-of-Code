import os


def _get_cycle_register_values(instructions: list[list[str]]) -> list[int]:
    # Creates a list where every entry with id c holds the value of the register X during cycle c
    result = [0]
    register = 1
    cycle = 1
    for instr in instructions:
        result.append(register)
        if instr[0] == 'addx':
            cycle += 1
            result.append(register)
            register += int(instr[1])
        cycle += 1
    return result


def part_a(puzzle_input: str) -> str:
    instructions = list(map(lambda i: i.split(), puzzle_input.strip().split('\n')))
    cycle_register_values = _get_cycle_register_values(instructions)

    signal_strength = 0
    for cycle in range(20, 221, 40):
        signal_strength += cycle * cycle_register_values[cycle]

    return str(signal_strength)


def part_b(puzzle_input: str) -> str:
    instructions = list(map(lambda i: i.split(), puzzle_input.strip().split('\n')))
    cycle_register_values = _get_cycle_register_values(instructions)

    result = '\n'
    for line in range(6):
        for cycle in range(1, 41):
            if cycle_register_values[40 * line + cycle] <= cycle <= cycle_register_values[40 * line + cycle] + 2:
                result += '#'
            else:
                result += '.'
        result += '\n'

    return result.rstrip()


if __name__ == '__main__':
    input_str = open(os.path.join(os.path.dirname(__file__), '..', 'input', 'input10.txt')).read()
    print(f'Part A: {part_a(input_str)}')
    print(f'Part B: {part_b(input_str)}')
