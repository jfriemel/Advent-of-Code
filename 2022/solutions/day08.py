import os


def part_a(puzzle_input: str) -> str:
    # Iterate over four different lists: left-to-right, right-to-left, bottom-to-top, top-to-bottom
    # Each tree is represented by its height and its (x,y) position in the grid
    input_1 = [list(map(int, s)) for s in map(list, puzzle_input.strip().split('\n'))]
    for y in range(len(input_1)):
        for x in range(len(input_1[0])):
            input_1[y][x] = (input_1[y][x], (x, y))
    input_2 = list(map(lambda s: list(reversed(s)), input_1))
    input_3 = list(map(list, zip(*input_1[::-1])))
    input_4 = list(map(lambda s: list(reversed(s)), input_3))

    visible = set()
    # Add boundary trees to set of visible trees
    for s in range(len(input_1)):
        visible.add((0, s))
        visible.add((len(input_1[0]) - 1, s))
    for s in range(len(input_1[0])):
        visible.add((s, 0))
        visible.add((s, len(input_1) - 1))

    for inp in [input_1, input_2, input_3, input_4]:
        for tree_line in inp:
            current_max = 0
            for (height, position) in tree_line:
                if height > current_max:
                    visible.add(position)
                    current_max = height
                    if height == 9:
                        break

    return str(len(visible))


def part_b(puzzle_input: str) -> str:
    tree_grid = [list(map(int, s)) for s in map(list, puzzle_input.strip().split('\n'))]

    # Calculate scenic scores for all trees
    max_scenic_score = 0
    for (i, j) in [(i, j) for i in range(len(tree_grid)) for j in range(len(tree_grid[0]))]:
        scenic_score = 1

        # Very repetitive code, but it works, so ...
        trees_visible = 0
        for dist in range(1, len(tree_grid[0]) - i):
            trees_visible += 1
            if tree_grid[i][j] <= tree_grid[i + dist][j]:
                break
        scenic_score *= trees_visible
        trees_visible = 0
        for dist in range(- 1, -i - 1, -1):
            trees_visible += 1
            if tree_grid[i][j] <= tree_grid[i + dist][j]:
                break
        scenic_score *= trees_visible
        trees_visible = 0
        for dist in range(1, len(tree_grid) - j):
            trees_visible += 1
            if tree_grid[i][j] <= tree_grid[i][j + dist]:
                break
        scenic_score *= trees_visible
        trees_visible = 0
        for dist in range(- 1, -j - 1, -1):
            trees_visible += 1
            if tree_grid[i][j] <= tree_grid[i][j + dist]:
                break
        scenic_score *= trees_visible
        if scenic_score > max_scenic_score:
            max_scenic_score = scenic_score

    return str(max_scenic_score)


if __name__ == '__main__':
    input_str = open(os.path.join(os.path.dirname(__file__), '..', 'input', 'input08.txt')).read()
    print(f'Part A: {part_a(input_str)}')
    print(f'Part B: {part_b(input_str)}')
