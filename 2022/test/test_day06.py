import pytest

from solutions.day06 import part_a, part_b


part_a_in_out = [('mjqjpqmgbljsphdztnvjfqwrcgsmlb', '7'), ('bvwbjplbgvbhsrlpgdmjqwftvncz', '5'),
                 ('nppdvjthqldpwncqszvftbrmjlhg', '6'), ('nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg', '10'),
                 ('zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw', '11')]
part_b_in_out = [('mjqjpqmgbljsphdztnvjfqwrcgsmlb', '19'), ('bvwbjplbgvbhsrlpgdmjqwftvncz', '23'),
                 ('nppdvjthqldpwncqszvftbrmjlhg', '23'), ('nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg', '29'),
                 ('zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw', '26')]


@pytest.mark.parametrize('test_input, expected', part_a_in_out)
def test_part_a(test_input, expected):
    assert part_a(test_input) == expected


@pytest.mark.parametrize('test_input, expected', part_b_in_out)
def test_part_b(test_input, expected):
    assert part_b(test_input) == expected
