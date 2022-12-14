import os


def create_rock_map(puzzle_input: str) -> (set, int):
    scans = map(lambda l:
                list(map(lambda p: tuple(map(int, p.split(','))), l.split(' -> '))),
                puzzle_input.strip().split('\n'))

    rocks = set()
    max_y = 0
    for scan in scans:
        (prev_x, prev_y) = scan.pop(0)
        for x, y in scan:
            if y > max_y:
                max_y = y
            for i in range(min(x, prev_x), max(x, prev_x) + 1):
                for j in range(min(y, prev_y), max(y, prev_y) + 1):
                    rocks.add((i, j))
            prev_x, prev_y = (x, y)

    return rocks, max_y


def count_sand(rocks: set, max_y: int, with_floor: bool) -> int:
    num_sand = 0
    while True:
        sand_pos = (500, 0)
        while sand_pos[1] < max_y:
            if (sand_pos[0], sand_pos[1] + 1) not in rocks:
                sand_pos = (sand_pos[0], sand_pos[1] + 1)
            elif (sand_pos[0] - 1, sand_pos[1] + 1) not in rocks:
                sand_pos = (sand_pos[0] - 1, sand_pos[1] + 1)
            elif (sand_pos[0] + 1, sand_pos[1] + 1) not in rocks:
                sand_pos = (sand_pos[0] + 1, sand_pos[1] + 1)
            elif sand_pos in rocks:
                return num_sand
            else:
                rocks.add(sand_pos)
                num_sand += 1
                break
        else:  # while loop was not broken, sand will fall indefinitely or hit the floor
            if with_floor and sand_pos not in rocks:
                rocks.add(sand_pos)
                num_sand += 1
            else:
                break
    return num_sand


def part_a(puzzle_input: str) -> str:
    rocks, max_y = create_rock_map(puzzle_input)
    return str(count_sand(rocks, max_y, False))


def part_b(puzzle_input: str) -> str:
    rocks, max_y = create_rock_map(puzzle_input)
    return str(count_sand(rocks, max_y + 1, True))


if __name__ == '__main__':
    input_str = open(os.path.join(os.path.dirname(__file__), '..', 'input', 'input14.txt')).read()
    print(f'Part A: {part_a(input_str)}')
    print(f'Part B: {part_b(input_str)}')
