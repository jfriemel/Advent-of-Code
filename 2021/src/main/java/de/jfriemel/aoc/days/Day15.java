package de.jfriemel.aoc.days;

import de.jfriemel.aoc.Day;

import java.util.*;

public class Day15 implements Day {

    // Using Integer.MAX_VALUE may cause an unnoticed overflow. This is big enough.
    private static final int INFINITY = 50000;

    @Override
    public String part1(List<String> input) {
        return Integer.toString(findShortestPath(input, 1));
    }

    @Override
    public String part2(List<String> input) {
        return Integer.toString(findShortestPath(input, 5));
    }

    private int findShortestPath(final List<String> input, final int factor) {
        final int[][] grid = getGrid(input, factor);
        final Graph15 G = new Graph15();

        // Add each value in the grid as a node to the graph.
        for (int i = 0; i < grid.length; i++) {
            for (int j = 0; j < grid[i].length; j++) {
                G.addVertex(i  + "," + j, grid[i][j]);
            }
        }

        // Add edges from each node to its four neighbors.
        for (int i = 1; i < grid.length - 1; i++) {
            for (int j = 1; j < grid[i].length - 1; j++) {
                G.addEdge(i + "," + j, (i-1) + "," + j);
                G.addEdge(i + "," + j, (i+1) + "," + j);
                G.addEdge(i + "," + j, i + "," + (j-1));
                G.addEdge(i + "," + j, i + "," + (j+1));
            }
        }

        final Map<Vertex15, Integer> pathLengthMap = new HashMap<>();
        final Deque<Vertex15> nodeStack = new LinkedList<>();
        final Vertex15 start = G.getVertex("1,1");

        pathLengthMap.put(start, 0);
        nodeStack.push(start);

        while (!nodeStack.isEmpty()) {
            Vertex15 v = nodeStack.pollLast();
            for (final Vertex15 w : G.getAdjacencies(v)) {
                if (pathLengthMap.getOrDefault(w, INFINITY) > pathLengthMap.get(v) + w.cost()) {
                    // If the path from v to w is better than all previous paths to w, choose the new path.
                    pathLengthMap.put(w, pathLengthMap.get(v) + w.cost());
                    nodeStack.push(w);
                }
            }
        }

        // The destination is in the bottom left corner (not including the INFINITY borders).
        return pathLengthMap.get(G.getVertex((grid.length - 2) + "," + (grid.length - 2)));
    }

    private int[][] getGrid(final List<String> input, final int factor) {
        final List<char[]> inputGrid = input.stream().map(String::toCharArray).toList();
        // Add borders with value INFINITY to all four sides to avoid annoying edge cases.
        final int[][] grid = new int[inputGrid.size() * factor + 2][inputGrid.get(0).length * factor + 2];
        for (final int[] row : grid) {
            Arrays.fill(row, INFINITY);
        }

        for (int fi = 0; fi < factor; fi++) {
            for (int i = 0; i < inputGrid.size(); i++) {
                for (int fj = 0; fj < factor; fj++) {
                    for (int j = 0; j < inputGrid.get(i).length; j++) {
                        int val = fi + fj + Integer.parseInt(inputGrid.get(i)[j] + "");
                        while (val > 9)
                            val -= 9;
                        grid[fi * inputGrid.size() + i + 1][fj * inputGrid.get(i).length + j + 1] = val;
                    }
                }
            }
        }

        return grid;
    }

}

class Graph15 {
    private final Map<Vertex15, Set<Vertex15>> adjacencies;
    private final Map<String, Vertex15> vertexLookup;

    Graph15() {
        adjacencies = new HashMap<>();
        vertexLookup = new HashMap<>();
    }

    void addVertex(final String label, final int cost) {
        Vertex15 vertex = new Vertex15(label, cost);
        adjacencies.computeIfAbsent(vertex, k -> new HashSet<>());
        vertexLookup.put(label, vertex);
    }

    void addEdge(final String label1, final String label2) {
        final Vertex15 v1 = vertexLookup.get(label1);
        final Vertex15 v2 = vertexLookup.get(label2);
        adjacencies.get(v1).add(v2);
        adjacencies.get(v2).add(v1);
    }

    Vertex15 getVertex(final String label) {
        return vertexLookup.get(label);
    }

    Set<Vertex15> getAdjacencies(final Vertex15 v) {
        return adjacencies.get(v);
    }
}

record Vertex15(String label, int cost) {}