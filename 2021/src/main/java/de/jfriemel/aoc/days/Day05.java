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

    private int findOverlaps(List<String> input, boolean considerDiags) {
        List<int[][]> coordList = parseInput(input);
        HashMap<String, Integer> overlapCounts = new HashMap<>();
        for (int[][] coords : coordList) {
            int minX = Math.min(coords[0][0], coords[1][0]);
            int minY = Math.min(coords[0][1], coords[1][1]);
            int maxX = Math.max(coords[0][0], coords[1][0]);
            int maxY = Math.max(coords[0][1], coords[1][1]);
            if (coords[0][0] == coords[1][0] || coords[0][1] == coords[1][1]) {
                for (int i = minX; i <= maxX; i++) {
                    for (int j = minY; j <= maxY; j++) {
                        String key = i + "," + j;
                        if (overlapCounts.containsKey(key)) {
                            overlapCounts.put(key, overlapCounts.get(key) + 1);
                        } else {
                            overlapCounts.put(key, 1);
                        }
                    }
                }
            } else if (considerDiags) {
                boolean ascX = coords[0][0] < coords[1][0];
                boolean ascY = coords[0][1] < coords[1][1];
                for (int i = 0; i + minX <= maxX; i++) {
                    String key = (coords[0][0] + (ascX ? i : -i)) + "," + (coords[0][1] + (ascY ? i : -i));
                    if (overlapCounts.containsKey(key)) {
                        overlapCounts.put(key, overlapCounts.get(key) + 1);
                    } else {
                        overlapCounts.put(key, 1);
                    }
                }
            }
        }
        int result = 0;
        for (String key : overlapCounts.keySet()) {
            if (overlapCounts.get(key) > 1) {
                result++;
            }
        }
        return result;
    }

    private List<int[][]> parseInput(List<String> input) {
        List<int[][]> coordList = new ArrayList<>();
        for (String line : input) {
            int[][] coords = new int[2][2];
            String[] parts = line.split(" -> ");
            String[] left = parts[0].split(",");
            String[] right = parts[1].split(",");
            coords[0][0] = Integer.parseInt(left[0]);
            coords[0][1] = Integer.parseInt(left[1]);
            coords[1][0] = Integer.parseInt(right[0]);
            coords[1][1] = Integer.parseInt(right[1]);
            coordList.add(coords);
        }
        return coordList;
    }

}
