import os


def part_a(puzzle_input: str) -> str:
    cubes = set(map(lambda l: tuple(map(int, l.split(','))), puzzle_input.strip().split('\n')))
    result = 0
    for x, y, z in cubes:
        for dx, dy, dz in [(1, 0, 0), (-1, 0, 0), (0, 1, 0), (0, -1, 0), (0, 0, 1), (0, 0, -1)]:
            if (x + dx, y + dy, z + dz) not in cubes:
                result += 1
    return str(result)


def part_b(puzzle_input: str) -> str:
    cubes = set(map(lambda l: tuple(map(int, l.split(','))), puzzle_input.strip().split('\n')))
    min_coord = min(min(x, y, z) for x, y, z in cubes) - 1
    max_coord = max(max(x, y, z) for x, y, z in cubes) + 1

    # DFS to find all positions around the droplet that are filled by air (i.e. find the outer boundary)
    visited = set()
    candidates = {(min_coord, min_coord, min_coord)}
    while len(candidates) > 0:
        (x, y, z) = candidates.pop()
        visited.add((x, y, z))
        if (x, y, z) in cubes:
            continue
        for (dx, dy, dz) in [(1, 0, 0), (-1, 0, 0), (0, 1, 0), (0, -1, 0), (0, 0, 1), (0, 0, -1)]:
            candidate = (x + dx, y + dy, z + dz)
            if all(min_coord <= p <= max_coord for p in candidate) and candidate not in visited:
                candidates.add(candidate)

    outer_air = visited - cubes
    result = 0
    for x, y, z in cubes:
        for dx, dy, dz in [(1, 0, 0), (-1, 0, 0), (0, 1, 0), (0, -1, 0), (0, 0, 1), (0, 0, -1)]:
            if (x + dx, y + dy, z + dz) in outer_air:  # only consider cube sides facing outside
                result += 1

    return str(result)


if __name__ == '__main__':
    input_str = open(os.path.join(os.path.dirname(__file__), '..', 'input', 'input18.txt')).read()
    print(f'Part A: {part_a(input_str)}')
    print(f'Part B: {part_b(input_str)}')
