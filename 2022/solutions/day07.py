import os
import random


class TreeNode:
    def __init__(self, name: str, size: int = 0):
        self.name = name
        self.size = size
        self.children = []
        self.parent = None
        self.key = random.random()


def _create_tree_node(name: str, parent: TreeNode | None, size: int = 0) -> TreeNode:
    tree_node = TreeNode(name, size)
    tree_node.parent = parent
    if parent is not None:
        parent.children.append(tree_node)
    return tree_node


def _get_node_sizes(tree_node: TreeNode, node_sizes: dict[(str, float), int]):
    current_size = 0
    for child in tree_node.children:
        if child.size > 0:
            current_size += child.size
        elif len(child.children) > 0:
            _get_node_sizes(child, node_sizes)
            current_size += node_sizes[(child.name, child.key)]
    node_sizes[(tree_node.name, tree_node.key)] = current_size


def _create_dir_tree(commands: list[str]) -> (TreeNode, dict[(str, float), int]):
    tree_node = None
    for command in map(lambda c: c.split(' '), commands):
        # create tree nodes based on current command
        if command[0] == '$':
            if command[1] == 'cd':
                if command[2] == '..' and tree_node is not None:
                    tree_node = tree_node.parent
                else:
                    tree_node = _create_tree_node(command[2], tree_node)
        else:
            for tn in tree_node.children:
                if tn.name == command[1]:
                    continue
            if command[0] == 'dir':
                _create_tree_node(command[1], tree_node)
            else:
                _create_tree_node(command[1], tree_node, int(command[0]))

    # find topmost directory ('/')
    while tree_node.parent is not None:
        tree_node = tree_node.parent

    # find size of all directories
    node_sizes = {}
    _get_node_sizes(tree_node, node_sizes)

    return tree_node, node_sizes


def part_a(puzzle_input: str) -> str:
    commands = puzzle_input.strip().split('\n')
    _, node_sizes = _create_dir_tree(commands)

    result = 0
    for k, v in node_sizes.items():
        if v <= 100000:
            result += v

    return str(result)


def part_b(puzzle_input: str) -> str:
    commands = puzzle_input.strip().split('\n')
    tree_node, node_sizes = _create_dir_tree(commands)

    available = 70000000 - node_sizes[(tree_node.name, tree_node.key)]
    current_best = 70000000
    for k, v in node_sizes.items():
        if available + v >= 30000000 and v < current_best:
            current_best = v

    return str(current_best)


if __name__ == '__main__':
    input_str = open(os.path.join(os.path.dirname(__file__), '..', 'input', 'input07.txt')).read()
    print(f'Part A: {part_a(input_str)}')
    print(f'Part B: {part_b(input_str)}')
