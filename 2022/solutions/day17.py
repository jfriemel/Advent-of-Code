import itertools
import os


def fall_rocks(movements: str, occupied: set, num_iter: int, check_repeat: bool) -> int:
    move_idx = 0
    max_y = max(y for _, y in occupied)
    prev_positions = {}

    i = -1
    while i < num_iter - 1:
        i += 1

        # determine next rock's starting position
        cur_y = max_y + 4
        match i % 5:
            case 0:
                new_rock = [(2, cur_y + 0), (3, cur_y + 0), (4, cur_y + 0), (5, cur_y + 0)]
            case 1:
                new_rock = [(2, cur_y + 1), (3, cur_y + 0), (3, cur_y + 1), (3, cur_y + 2), (4, cur_y + 1)]
            case 2:
                new_rock = [(2, cur_y + 0), (3, cur_y + 0), (4, cur_y + 0), (4, cur_y + 1), (4, cur_y + 2)]
            case 3:
                new_rock = [(2, cur_y + 0), (2, cur_y + 1), (2, cur_y + 2), (2, cur_y + 3)]
            case _:
                new_rock = [(2, cur_y + 0), (2, cur_y + 1), (3, cur_y + 0), (3, cur_y + 1)]

        while True:  # movement loop, gets broken once the rock cannot fall further
            # move sideways
            movement = movements[move_idx]
            move_idx = (move_idx + 1) % len(movements)
            if movement == '>':
                test_rock = [(x + 1, y) for x, y in new_rock]
            else:
                test_rock = [(x - 1, y) for x, y in new_rock]
            for x, y in test_rock:
                if x < 0 or x > 6 or (x, y) in occupied:  # collision detected
                    break
            else:  # for loop was not broken, new position is legal
                new_rock = test_rock

            # move downwards
            test_rock = [(x, y - 1) for x, y in new_rock]
            for pos in test_rock:
                if pos in occupied:  # collision detected
                    occupied.update(set(new_rock))
                    max_y = max(y for _, y in occupied)
                    break
            else:  # for loop was not broken, new position is legal
                new_rock = test_rock
                continue
            break

        if not check_repeat:
            continue

        # generate top-down view of rocks
        maxes = {x: 0 for x in range(7)}
        for x, y in occupied:
            if y > maxes[x]:
                maxes[x] = y
        topmost = tuple((x, maxes[x] - max_y) for x in maxes)

        # check if we had the same scenario before,
        # i.e. same topmost rocks, same movement index and same rock falling down next
        key = (topmost, move_idx, i % 5)
        if key in prev_positions:
            prev_i, prev_max, prev_occupied = prev_positions[key]
            factor = (num_iter - i) // (i - prev_i)
            max_y += factor * (max_y - prev_max)
            i += factor * (i - prev_i)
            occupied = {(x, y - prev_max + max_y) for x, y in prev_occupied}

        # store current iteration, maximum height and occupied cells
        prev_positions[key] = (i, max_y, occupied.copy())

    return max_y


def part_a(puzzle_input: str) -> str:
    movements = puzzle_input.strip()
    occupied = {(x, 0) for x in range(7)}
    return str(fall_rocks(movements, occupied, 2022, False))


def part_b(puzzle_input: str) -> str:
    movements = puzzle_input.strip()
    occupied = {(x, 0) for x in range(7)}
    return str(fall_rocks(movements, occupied, 1_000_000_000_000, True))


if __name__ == '__main__':
    input_str = open(os.path.join(os.path.dirname(__file__), '..', 'input', 'input17.txt')).read()
    print(f'Part A: {part_a(input_str)}')
    print(f'Part B: {part_b(input_str)}')
