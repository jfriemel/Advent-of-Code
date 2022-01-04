package de.jfriemel.aoc.days;

import de.jfriemel.aoc.Day;

import java.util.Arrays;
import java.util.List;

public class Day07 implements Day {

    @Override
    public String part1(List<String> input) {
        // The total fuel cost is minimal at the median position of the crabs.
        final List<Integer> crabList = Arrays.stream(input.get(0).split(","))
                                             .map(Integer::parseInt)
                                             .sorted()
                                             .toList();
        return Integer.toString(getTotalCost(crabList, crabList.get(crabList.size() / 2), false));
    }

    @Override
    public String part2(List<String> input) {
        // The total fuel cost is minimal around the mean position of the crabs (+- 1).
        final List<Integer> crabList = Arrays.stream(input.get(0).split(","))
                                             .map(Integer::parseInt)
                                             .sorted()
                                             .toList();
        final int mean = (int) Math.round(crabList.stream().mapToDouble(c -> c).average().orElseThrow());
        return Integer.toString(Math.min(getTotalCost(crabList, mean - 1, true),
                                         getTotalCost(crabList, mean, true)),
                                         getTotalCost(crabList, mean + 1, true));
    }

    private int getTotalCost(final List<Integer> crabList, final int alignPos, final boolean expensive) {
        // Computes the total fuel cost for a given alignment position.
        return crabList.stream().map(c -> getSingleCost(c, alignPos, expensive)).reduce(Integer::sum).orElseThrow();
    }

    private int getSingleCost(int crabPos, int alignPos, boolean expensive) {
        // Computes the fuel cost for a single crab to reach a given alignment position.
        // If expensive is true, the cost rate becomes greater when the travel distance increases.
        int n = Math.abs(crabPos - alignPos);
        return expensive ? n * (n + 1) / 2 : n;
    }

}