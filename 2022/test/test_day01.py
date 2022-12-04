from solutions.day01 import part_a, part_b

test_input = '''1000
2000
3000

4000

5000
6000

7000
8000
9000

10000
'''


def test_part_a():
    assert part_a(test_input) == '24000'


def test_part_b():
    assert part_b(test_input) == '45000'
