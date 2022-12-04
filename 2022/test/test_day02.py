from solutions.day02 import part_a, part_b

test_input = '''A Y
B X
C Z
'''


def test_part_a():
    assert part_a(test_input) == '15'


def test_part_b():
    assert part_b(test_input) == '12'
