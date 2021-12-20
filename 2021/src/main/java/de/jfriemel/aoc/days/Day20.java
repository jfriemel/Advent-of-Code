package de.jfriemel.aoc.days;

import de.jfriemel.aoc.Day;

import java.util.*;

public class Day20 implements Day {

    // When DEBUG is true, the image is printed at every step.
    private static final boolean DEBUG = false;

    @Override
    public String part1(List<String> input) {
        return Integer.toString(enhance(input, 2));
    }

    @Override
    public String part2(List<String> input) {
        return Integer.toString(enhance(input, 50));
    }

    private int enhance(final List<String> input, final int numOfIterations) {
        final String rules = input.get(0);
        Map<String, Character> fieldMap = parseImage(input.subList(2, input.size()));

        if (DEBUG) {
            printImage(fieldMap, '.');
        }

        // unvisited is the character assigned to fields that have not been visited yet.
        // When rules[0] is # and rules[511] is ., the unvisited fields switch between # and . at every iteration.
        char unvisited = '.';

        for (int iteration = 0; iteration < numOfIterations; iteration++) {
            Set<String> fields = getFields(fieldMap.keySet());

            final Map<String, Character> nextMap = new HashMap<>();
            for (final String field : fields) {
                final int[] pos = fieldToPos(field);

                int val = 0;
                for (int y = pos[1] - 1; y <= pos[1] + 1; y++) {
                    for (int x = pos[0] - 1; x <= pos[0] + 1; x++) {
                        // Parse the binary value.
                        val = val * 2 + (fieldMap.getOrDefault(x + "," + y, unvisited) == '#' ? 1 : 0);
                    }
                }
                nextMap.put(field, rules.charAt(val));
            }
            fieldMap = nextMap;

            if (DEBUG) {
                printImage(fieldMap, unvisited);
            }

            // Switch unvisited between # and . when required by the input rules.
            if (unvisited == '.' && rules.charAt(0) == '#') {
                unvisited = '#';
            } else if (unvisited == '#' && rules.charAt(511) == '.') {
                unvisited = '.';
            }
        }

        int countLit = 0;
        for (final String field : fieldMap.keySet()) {
            if (fieldMap.get(field) == '#') {
                countLit++;
            }
        }
        return countLit;
    }

    private Map<String, Character> parseImage(final List<String> input) {
        Map<String, Character> fieldMap = new HashMap<>();
        for (int y = 0; y < input.size(); y++) {
            for (int x = 0; x < input.get(y).length(); x++) {
                if (input.get(y).charAt(x) == '#') {
                    // Save some space and time by only adding # fields to the initial map.
                    fieldMap.put(x + "," + y, input.get(y).charAt(x));
                }
            }
        }
        return fieldMap;
    }

    private Set<String> getFields(final Set<String> fieldsInMap) {
        // Returns a set of fields that need to be examined in the next iteration.
        // The set contains all fields adjacent to fields from the previous map.
        Set<String> fields = new HashSet<>();
        for (final String field : fieldsInMap) {
            final int[] pos = fieldToPos(field);
            for (int x = pos[0] - 1; x <= pos[0] + 1; x++) {
                for (int y = pos[1] - 1; y <= pos[1] + 1; y++) {
                    fields.add(x + "," + y);
                }
            }
        }
        return fields;
    }

    private int[] fieldToPos(final String field) {
        // Helper method, that takes a field string and returns an array containing the x and y coordinate.
        final String[] parts = field.split(",");
        return new int[]{Integer.parseInt(parts[0]), Integer.parseInt(parts[1])};
    }

    private void printImage(final Map<String, Character> fieldMap, final char unvisited) {
        // Prints the image, useful for debugging.
        final Set<String> fields = fieldMap.keySet();
        int minX = Integer.MAX_VALUE;
        int minY = Integer.MAX_VALUE;
        int maxX = Integer.MIN_VALUE;
        int maxY = Integer.MIN_VALUE;
        for (final String field : fields) {
            final int[] pos = fieldToPos(field);
            if (pos[0] < minX) minX = pos[0];
            if (pos[0] > maxX) maxX = pos[0];
            if (pos[1] < minY) minY = pos[1];
            if (pos[1] > maxY) maxY = pos[1];
        }

        for (int y = minY; y <= maxY; y++) {
            for (int x = minX; x <= maxX; x++) {
                System.out.print(fieldMap.getOrDefault(x + "," + y, unvisited));
            }
            System.out.println();
        }
        System.out.println();
    }

}