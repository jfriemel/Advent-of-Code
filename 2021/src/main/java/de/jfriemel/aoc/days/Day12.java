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
        final Graph12 G = new Graph12();
        for (final String line : input) {
            String[] vertices = line.split("-");
            G.addEdge(vertices[0], vertices[1]);
        }
        final Vertex12 twice = allowTwice ? null : new Vertex12("start");
        return traverse(G, new ArrayList<>(), new Vertex12("start"), 0, twice);
    }

    private int traverse(final Graph12 G, final List<Vertex12> visited, final Vertex12 current, int pathCount,
                         final Vertex12 twice) {
        if (current.equals(new Vertex12("end"))) {
            if (twice == null || visited.contains(twice))
                pathCount++;
            return pathCount;
        }

        if (!current.isBig()) {
            if (visited.contains(current))
                return pathCount;
            if (twice == null && !current.equals(new Vertex12("start"))) {
                for (final Vertex12 next : G.getAdjacencies(current)) {
                    final List<Vertex12> visitedNext = new ArrayList<>(visited);
                    pathCount = traverse(G, visitedNext, next, pathCount, current);
                }
            }
            visited.add(current);
        }

        for (final Vertex12 next : G.getAdjacencies(current)) {
            List<Vertex12> visitedNext = new ArrayList<>(visited);
            pathCount = traverse(G, visitedNext, next, pathCount, twice);
        }

        return pathCount;
    }

}

class Graph12 {
    private final Map<Vertex12, List<Vertex12>> adjacencies;

    Graph12() {
        adjacencies = new HashMap<>();
    }

    Vertex12 addVertex(final String label) {
        Vertex12 vertex = new Vertex12(label);
        adjacencies.computeIfAbsent(vertex, k -> new ArrayList<>());
        return vertex;
    }

    void addEdge(final String label1, final String label2) {
        final Vertex12 v1 = addVertex(label1);
        final Vertex12 v2 = addVertex(label2);
        adjacencies.get(v1).add(v2);
        adjacencies.get(v2).add(v1);
    }

    List<Vertex12> getAdjacencies(final Vertex12 v) {
        return adjacencies.get(v);
    }
}

class Vertex12 {
    private final String label;
    private final boolean big;

    Vertex12(final String label) {
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
        return this.label.equals(((Vertex12) obj).label);
    }
}