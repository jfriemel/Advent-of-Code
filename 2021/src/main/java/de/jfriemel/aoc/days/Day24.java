package de.jfriemel.aoc.days;

import de.jfriemel.aoc.Day;

import java.util.ArrayList;
import java.util.Deque;
import java.util.LinkedList;
import java.util.List;

public class Day24 implements Day {

    // My first solution was a brute force calculation over all possible inputs from 999...9 downwards and 111...1
    // upwards until an input resulted in a 0. Surprisingly, with a couple of optimizations, it actually worked and
    // returned the correct input digits. But it took many hours to run. Not very nice.
    // I then looked at other people's approaches and found this neat solution where the input digits are computed
    // directly:
    // https://github.com/mebeim/aoc/tree/master/2021#day-24---arithmetic-logic-unit.
    // I am now using the same approach as described in the repo above for this day's solution.

    // Note that while this code _should_ work on all AoC inputs, it does not work with arbitrary ALU programs.

    @Override
    public String part1(List<String> input) {
        final int[][] inputConstants = parseInputConstants(input);
        return Long.toString(solveConstraints(inputConstants[0], inputConstants[1], inputConstants[2], true));
    }

    @Override
    public String part2(List<String> input) {
        final int[][] inputConstants = parseInputConstants(input);
        return Long.toString(solveConstraints(inputConstants[0], inputConstants[1], inputConstants[2], false));
    }

    private int[][] parseInputConstants(final List<String> input) {
        // The input program basically consists of 18 lines that are repeated 14 times with only three constants changed
        // at each repetition. This method parses the constants.
        final int partSize = input.size() / 14;
        final List<List<String>> partitions = new ArrayList<>(14);
        for (int i = 0; i < input.size(); i += partSize) {
            partitions.add(input.subList(i, i + partSize));
        }

        int[] addX = new int[14];
        int[] addY = new int[14];
        int[] divZ = new int[14];
        for (int i = 0; i < 14; i++) {
            addX[i] = Integer.parseInt(partitions.get(i).get(5).split(" ")[2]);
            addY[i] = Integer.parseInt(partitions.get(i).get(15).split(" ")[2]);
            divZ[i] = Integer.parseInt(partitions.get(i).get(4).split(" ")[2]);
        }

        return new int[][]{addX, addY, divZ};
    }

    long solveConstraints(final int[] addX, final int[] addY, final int[] divZ, final boolean max) {
        final List<int[]> constraintList = new ArrayList<>();
        final Deque<int[]> constantStack = new LinkedList<>();

        for (int i = 0; i < 14; i++) {
            if (divZ[i] == 1) {
                constantStack.add(new int[]{i, addY[i]});
            } else {
                final int[] prev = constantStack.pollLast();
                // prev = {index j, addY[j]}.
                constraintList.add(new int[]{i, prev[0], prev[1] + addX[i]});
                // new list element = {index i, index j, addY[j] + addX[i]}.
                // Note that i > j.
            }
        }

        final int[] digits = new int[14];
        for (final int[] constraint : constraintList) {
            // Remember: constraint = {index i, index j, addY[j] + addX[i]}.
            // Any solution must satisfy: digits[i] - digits[j] = addY[j] + addX[i].
            // To maximize the result, ensure that one of the digits is always 9 (highest possible).
            // To minimize the result, ensure that one of the digits is always 1 (lowest possible).
            if (constraint[2] > 0) {
                if (max) {
                    digits[constraint[0]] = 9;
                    digits[constraint[1]] = 9 - constraint[2];
                } else {
                    digits[constraint[0]] = 1 + constraint[2];
                    digits[constraint[1]] = 1;
                }
            } else {
                if (max) {
                    digits[constraint[0]] = 9 + constraint[2];
                    digits[constraint[1]] = 9;
                } else {
                    digits[constraint[0]] = 1;
                    digits[constraint[1]] = 1 - constraint[2];
                }
            }
        }

        long result = 0L;
        for (final int digit : digits) {
            result = result * 10L + digit;
        }

        return result;
    }

}