from solutions.day25 import part_a, snafu_to_decimal

test_input = '''1=-0-2
12111
2=0=
21
2=01
111
20012
112
1=-1=
1-12
12
1=
122'''


def test_snafu_to_decimal():
    assert sum(map(snafu_to_decimal, test_input.split('\n'))) == 4890


def test_part_a():
    assert part_a(test_input) == '2=-1=0'
