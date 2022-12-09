import os


movement_dirs = {'U': (0, 1), 'D': (0, -1), 'L': (-1, 0), 'R': (1, 0)}


def part_a(puzzle_input: str) -> str:
    commands = list(map(lambda s: (movement_dirs[s.split()[0]], int(s.split()[1])), puzzle_input.strip().split('\n')))
    # A command looks like this: ((dx, dy), num_of_moves)
    current_t = current_h = (0, 0)
    visited_by_t = {(0, 0)}

    for command in commands:
        for i in range(command[1]):
            current_h = (current_h[0] + command[0][0], current_h[1] + command[0][1])
            diff = [current_h[0] - current_t[0], current_h[1] - current_t[1]]
            if not (-1 <= diff[0] <= 1 and -1 <= diff[1] <= 1):
                # If T and H are not touching, move T towards H, but no more than one block per direction (X, Y)
                if diff[0] != 0:
                    diff[0] //= abs(diff[0])
                if diff[1] != 0:
                    diff[1] //= abs(diff[1])
                current_t = (current_t[0] + diff[0], current_t[1] + diff[1])
                visited_by_t.add(current_t)

    return str(len(visited_by_t))


def part_b(puzzle_input: str) -> str:
    commands = list(map(lambda s: (movement_dirs[s.split()[0]], int(s.split()[1])), puzzle_input.strip().split('\n')))
    current_knots = [(0, 0)] * 10  # now use a list of knots instead of two distinct variables
    visited_by_tail = {(0, 0)}

    for command in commands:
        for i in range(command[1]):
            current_knots[0] = (current_knots[0][0] + command[0][0], current_knots[0][1] + command[0][1])
            for j in range(1, 10):
                diff = [current_knots[j-1][0] - current_knots[j][0], current_knots[j-1][1] - current_knots[j][1]]
                if not (-1 <= diff[0] <= 1 and -1 <= diff[1] <= 1):
                    if diff[0] != 0:
                        diff[0] //= abs(diff[0])
                    if diff[1] != 0:
                        diff[1] //= abs(diff[1])
                    current_knots[j] = (current_knots[j][0] + diff[0], current_knots[j][1] + diff[1])
            visited_by_tail.add(current_knots[9])

    return str(len(visited_by_tail))


if __name__ == '__main__':
    input_str = open(os.path.join(os.path.dirname(__file__), '..', 'input', 'input09.txt')).read()
    print(f'Part A: {part_a(input_str)}')
    print(f'Part B: {part_b(input_str)}')
