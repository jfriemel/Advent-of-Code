from solutions.day05 import part_a, part_b

test_input = r'''    [D]    
[N] [C]    
[Z] [M] [P]
 1   2   3 

move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2
'''


def test_part_a():
    assert part_a(test_input) == 'CMZ'


def test_part_b():
    assert part_b(test_input) == 'MCD'
