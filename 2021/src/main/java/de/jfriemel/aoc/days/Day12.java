package de.jfriemel.aoc.days;

import de.jfriemel.aoc.Day;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class Day12 implements Day {

    @Override
    public String part1(List<String> input) {
        return Integer.toString(countPaths(input, false));
    }

    @Override
    public String part2(List<String> input) {
        return Integer.toString(countPaths(input, true));
    }

    private int countPaths(final List<String> input, final boolean allowTwice) {
        final Graph G = new Graph();
        for (final String line : input) {
            String[] vertices = line.split("-");
            G.addEdge(vertices[0], vertices[1]);
        }
        final Vertex twice = allowTwice ? null : new Vertex("start");
        return traverse(G, new ArrayList<>(), new Vertex("start"), 0, twice);
    }

    private int traverse(final Graph G, final List<Vertex> visited, final Vertex current, int pathCount,
                         final Vertex twice) {
        if (current.equals(new Vertex("end"))) {
            if (twice == null || visited.contains(twice))
                pathCount++;
        } else {
            if (!current.isBig()) {
                if (visited.contains(current)) return pathCount;
                if (twice == null && !current.equals(new Vertex("start"))) {
                    for (final Vertex next : G.getAdjacencies(current)) {
                        List<Vertex> visitedNext = new ArrayList<>(visited);
                        pathCount = traverse(G, visitedNext, next, pathCount, current);
                    }
                }
                visited.add(current);
            }
            for (final Vertex next : G.getAdjacencies(current)) {
                List<Vertex> visitedNext = new ArrayList<>(visited);
                pathCount = traverse(G, visitedNext, next, pathCount, twice);
            }
        }
        return pathCount;
    }

}

class Graph {
    private final Map<Vertex, List<Vertex>> adjacencies;

    Graph() {
        adjacencies = new HashMap<>();
    }

    Vertex addVertex(final String label) {
        Vertex vertex = new Vertex(label);
        adjacencies.computeIfAbsent(vertex, k -> new ArrayList<>());
        return vertex;
    }

    void addEdge(final String label1, final String label2) {
        final Vertex v1 = addVertex(label1);
        final Vertex v2 = addVertex(label2);
        adjacencies.get(v1).add(v2);
        adjacencies.get(v2).add(v1);
    }

    List<Vertex> getAdjacencies(final Vertex v) {
        return adjacencies.get(v);
    }
}

class Vertex {
    private final String label;
    private final boolean big;

    Vertex(final String label) {
        this.label = label;
        this.big = Character.isUpperCase(label.charAt(0));
    }

    boolean isBig() {
        return this.big;
    }

    @Override
    public int hashCode() {
        return label.hashCode();
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null || this.getClass() != obj.getClass())
            return false;
        return this.label.equals(((Vertex) obj).label);
    }
}