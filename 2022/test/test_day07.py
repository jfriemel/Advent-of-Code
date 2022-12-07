from solutions.day07 import part_a, part_b


test_input = '''$ cd /
$ ls
dir a
14848514 b.txt
8504156 c.dat
dir d
$ cd a
$ ls
dir e
29116 f
2557 g
62596 h.lst
$ cd e
$ ls
584 i
$ cd ..
$ cd ..
$ cd d
$ ls
4060174 j
8033020 d.log
5626152 d.ext
7214296 k'''


def test_part_a():
    assert part_a(test_input) == '95437'


def test_part_b():
    assert part_b(test_input) == '24933642'
