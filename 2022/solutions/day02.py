import os


def part_a(puzzle_input: str) -> str:
    games = map(lambda g: (ord(g[0]) - ord('A'), ord(g[2]) - ord('X')), puzzle_input.strip().split('\n'))
    score = 0
    for (player_a, player_b) in games:
        score += ((player_b - player_a + 1) % 3) * 3 + player_b + 1
    return str(score)


def part_b(puzzle_input: str) -> str:
    games = map(lambda g: (ord(g[0]) - ord('A'), ord(g[2]) - ord('X')), puzzle_input.strip().split('\n'))
    score = 0
    for (player_a, result) in games:
        player_b = (player_a + result + 2) % 3
        score += ((player_b - player_a + 1) % 3) * 3 + player_b + 1
    return str(score)


if __name__ == '__main__':
    input_str = open(os.path.join(os.path.dirname(__file__), '..', 'input', 'input02.txt')).read()
    print(f'Part A: {part_a(input_str)}')
    print(f'Part B: {part_b(input_str)}')
