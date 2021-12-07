package de.jfriemel.aoc.days;

import de.jfriemel.aoc.Day;

import java.util.Arrays;
import java.util.List;

public class Day07 implements Day {

    @Override
    public String part1(List<String> input) {
        List<Integer> crabList = Arrays.stream(input.get(0).split(",")).map(Integer::parseInt).sorted().toList();
        return Integer.toString(getTotalCost(crabList, crabList.get(crabList.size() / 2), false));
    }

    @Override
    public String part2(List<String> input) {
        List<Integer> crabList = Arrays.stream(input.get(0).split(",")).map(Integer::parseInt).sorted().toList();
        double mean = crabList.stream().mapToDouble(c -> c).average().orElseThrow();
        return Integer.toString(Math.min(getTotalCost(crabList, (int) Math.floor(mean), true),
                                         getTotalCost(crabList, (int) Math.ceil(mean), true)));
    }

    private int getTotalCost(List<Integer> crabList, int alignValue, boolean expensive) {
        return crabList.stream().map(c -> getSingleCost(c, alignValue, expensive)).reduce(Integer::sum).orElseThrow();
    }

    private int getSingleCost(int crabPos, int alignValue, boolean expensive) {
        int n = Math.abs(crabPos - alignValue);
        return expensive ? n * (n + 1) / 2 : n;
    }

}