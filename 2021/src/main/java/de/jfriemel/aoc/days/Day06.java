package de.jfriemel.aoc.days;

import de.jfriemel.aoc.Day;

import java.math.BigInteger;
import java.util.Arrays;
import java.util.List;
import java.util.stream.IntStream;
import java.util.stream.LongStream;

public class Day06 implements Day {

    @Override
    public String part1(List<String> input) {
        return Long.toString(simulate(input, 80));
    }

    @Override
    public String part2(List<String> input) {
        return Long.toString(simulate(input, 256));
    }

    private long simulate(List<String> input, int days) {
        List<Integer> initialLanternfish = Arrays.stream(input.get(0).split(",")).map(Integer::parseInt).toList();
        long[] lanternCounts = new long[]{0, 0, 0, 0, 0, 0, 0, 0, 0};
        for (final int lanternfish : initialLanternfish) {
            lanternCounts[lanternfish]++;
        }
        for (int i = 0; i < days; i++) {
            lanternCounts = simulateOnce(lanternCounts);
        }
        return LongStream.of(lanternCounts).sum();
    }

    private long[] simulateOnce(long[] previous) {
        long[] next = new long[]{0, 0, 0, 0, 0, 0, 0, 0, 0};
        for (int i = 0; i < previous.length; i++) {
            if (i > 0) {
                next[i - 1] += previous[i];
            } else {
                next[6] += previous[0];
                next[8] += previous[0];
            }
        }
        return next;
    }

}