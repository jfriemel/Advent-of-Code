from solutions.day12 import part_a, part_b


test_input = '''Sabqponm
abcryxxl
accszExk
acctuvwj
abdefghi'''


def test_part_a():
    assert part_a(test_input) == '31'


def test_part_b():
    assert part_b(test_input) == '29'
