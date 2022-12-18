import pytest

from solutions.day18 import part_a, part_b


test_input_1 = '''1,1,1
2,1,1'''

test_input_2 = '''2,2,2
1,2,2
3,2,2
2,1,2
2,3,2
2,2,1
2,2,3
2,2,4
2,2,6
1,2,5
3,2,5
2,1,5
2,3,5'''


@pytest.mark.parametrize('test_input, expected', [(test_input_1, '10'), (test_input_2, '64')])
def test_part_a(test_input, expected):
    assert part_a(test_input) == expected


@pytest.mark.parametrize('test_input, expected', [(test_input_1, '10'), (test_input_2, '58')])
def test_part_b(test_input, expected):
    assert part_b(test_input) == expected
