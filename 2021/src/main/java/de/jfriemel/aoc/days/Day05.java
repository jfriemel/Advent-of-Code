package de.jfriemel.aoc.days;

import de.jfriemel.aoc.Day;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

public class Day05 implements Day {

    @Override
    public String part1(List<String> input) {
        return Integer.toString(findOverlaps(input, false));
    }

    @Override
    public String part2(List<String> input) {
        return Integer.toString(findOverlaps(input, true));
    }

    private int findOverlaps(final List<String> input, final boolean considerDiags) {
        final List<int[][]> coordList = parseInput(input);
        final HashMap<String, Integer> overlapCounts = new HashMap<>();
        // The map keys are strings of the form "x,y" where x and y are the coordinates on the corresponding axes.

        for (final int[][] coords : coordList) {
            // Only check the coordinates that are actually on the vent:
            final int minX = Math.min(coords[0][0], coords[1][0]);
            final int minY = Math.min(coords[0][1], coords[1][1]);
            final int maxX = Math.max(coords[0][0], coords[1][0]);
            final int maxY = Math.max(coords[0][1], coords[1][1]);

            if (minX == maxX || minY == maxY) {
                // If the value on one of the axes is constant, the vent line is horizontal or vertical, not diagonal.
                for (int i = minX; i <= maxX; i++) {
                    for (int j = minY; j <= maxY; j++) {
                        final String key = i + "," + j;
                        if (overlapCounts.containsKey(key)) {
                            overlapCounts.put(key, overlapCounts.get(key) + 1);
                        } else {
                            overlapCounts.put(key, 1);
                        }
                    }
                }
            } else if (considerDiags) {
                final boolean ascX = coords[0][0] < coords[1][0]; // True if the x coordinates are ascending.
                final boolean ascY = coords[0][1] < coords[1][1]; // True if the y coordinates are ascending.
                for (int i = 0; i + minX <= maxX; i++) {
                    final String key = (coords[0][0] + (ascX ? i : -i)) + "," + (coords[0][1] + (ascY ? i : -i));
                    if (overlapCounts.containsKey(key)) {
                        overlapCounts.put(key, overlapCounts.get(key) + 1);
                    } else {
                        overlapCounts.put(key, 1);
                    }
                }
            }
        }
        int result = 0;
        for (final String key : overlapCounts.keySet()) {
            if (overlapCounts.get(key) > 1) {
                result++;
            }
        }
        return result;
    }

    private List<int[][]> parseInput(final List<String> input) {
        final List<int[][]> coordList = new ArrayList<>();
        // Each entry is a 2x2 array with the following layout:
        //   [0][_]: Starting point of the line.
        //   [1][_]: End point of the line.
        //   [_][0]: x coordinate of the current line point.
        //   [_][1]: y coordinate of the current line point.

        for (final String line : input) {
            final int[][] coords = new int[2][2];
            final String[] parts = line.split(" -> ");
            final String[] left  = parts[0].split(",");
            final String[] right = parts[1].split(",");

            coords[0][0] = Integer.parseInt(left[0]);
            coords[0][1] = Integer.parseInt(left[1]);
            coords[1][0] = Integer.parseInt(right[0]);
            coords[1][1] = Integer.parseInt(right[1]);
            coordList.add(coords);
        }
        return coordList;
    }

}