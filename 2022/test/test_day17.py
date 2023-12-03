from solutions.day17 import part_a, part_b


test_input = '>>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>'


def test_part_a():
    assert part_a(test_input) == '3068'


def test_part_b():
    assert part_b(test_input) == '1514285714288'
