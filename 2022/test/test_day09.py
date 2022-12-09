import pytest

from solutions.day09 import part_a, part_b


test_input_1 = '''R 4
U 4
L 3
D 1
R 4
D 1
L 5
R 2'''

test_input_2 = '''R 5
U 8
L 8
D 3
R 17
D 10
L 25
U 20'''


def test_part_a():
    assert part_a(test_input_1) == '13'


@pytest.mark.parametrize("test_input, expected", [(test_input_1, '1'), (test_input_2, '36')])
def test_part_b(test_input, expected):
    assert part_b(test_input) == expected
