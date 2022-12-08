from solutions.day08 import part_a, part_b


test_input = '''30373
25512
65332
33549
35390'''


def test_part_a():
    assert part_a(test_input) == '21'


def test_part_b():
    assert part_b(test_input) == '8'
