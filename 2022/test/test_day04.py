from solutions.day04 import part_a, part_b

test_input = '''2-4,6-8
2-3,4-5
5-7,7-9
2-8,3-7
6-6,4-6
2-6,4-8
'''


def test_part_a():
    assert part_a(test_input) == '2'


def test_part_b():
    assert part_b(test_input) == '4'
