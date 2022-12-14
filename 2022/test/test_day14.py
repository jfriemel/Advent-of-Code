from solutions.day14 import part_a, part_b


test_input = '''498,4 -> 498,6 -> 496,6
503,4 -> 502,4 -> 502,9 -> 494,9'''


def test_part_a():
    assert part_a(test_input) == '24'


def test_part_b():
    assert part_b(test_input) == '93'
