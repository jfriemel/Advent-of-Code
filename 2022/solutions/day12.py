import os
from collections import defaultdict


class Graph:
    def __init__(self, nodes: dict[(int, int), int]):
        self.nodes = nodes
        self.adjacencies = {}
        self._create_adjacencies()

    def _create_adjacencies(self):
        self.adjacencies = defaultdict(list)
        for (i, j), height in self.nodes.items():
            for di, dj in [(1, 0), (-1, 0), (0, 1), (0, -1)]:
                if self.nodes.get((i+di, j+dj), 2**32) <= height + 1:
                    # Invert adjacencies. We will start at the end point and find the shortest paths to the start points
                    self.adjacencies[(i+di, j+dj)].append((i, j))


def create_graph(puzzle_input: str) -> (dict[(int, int), int], (int, int), (int, int)):
    input_grid = list(map(lambda s: list(map(lambda c: ord(c) - ord('a'), s)), puzzle_input.strip().split('\n')))
    grid_dict = {}
    start = (0, 0)
    end = (0, 0)
    for i in range(len(input_grid)):
        for j in range(len(input_grid[0])):
            if input_grid[i][j] == -14:  # S
                start = (i, j)
                grid_dict[start] = 0
            elif input_grid[i][j] == -28:  # E
                end = (i, j)
                grid_dict[end] = 25
            else:
                grid_dict[(i, j)] = input_grid[i][j]
    return Graph(grid_dict), start, end


def dijkstra(graph: Graph, end: (int, int)) -> dict[(int, int), int]:
    # Since the graph is inverted, Dijkstra will find the shortest path from the end to any possible start point
    shortest_paths = {node: 2**32 for node in graph.nodes}
    shortest_paths[end] = 0
    unvisited_nodes = list(graph.nodes)

    while len(unvisited_nodes) > 0:
        current_min = next(iter(unvisited_nodes))
        for node in unvisited_nodes:
            if shortest_paths[node] < shortest_paths[current_min]:
                current_min = node
        for nbr in graph.adjacencies[current_min]:
            path_via_current = shortest_paths[current_min] + 1
            if shortest_paths[nbr] > path_via_current:
                shortest_paths[nbr] = path_via_current
        unvisited_nodes.remove(current_min)

    return shortest_paths


def part_a(puzzle_input: str) -> str:
    graph, start, end = create_graph(puzzle_input)
    return str(dijkstra(graph, end)[start])


def part_b(puzzle_input: str) -> str:
    graph, _, end = create_graph(puzzle_input)
    current_best = 2**32
    shortest_paths = dijkstra(graph, end)
    for node, height in graph.nodes.items():
        if height == 0 and shortest_paths[node] < current_best:
            current_best = shortest_paths[node]
    return str(current_best)


if __name__ == '__main__':
    input_str = open(os.path.join(os.path.dirname(__file__), '..', 'input', 'input12.txt')).read()
    print(f'Part A: {part_a(input_str)}')
    print(f'Part B: {part_b(input_str)}')
