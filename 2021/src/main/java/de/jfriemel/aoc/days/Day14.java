package de.jfriemel.aoc.days;

import de.jfriemel.aoc.Day;

import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class Day14 implements Day {

    @Override
    public String part1(List<String> input) {
        return Long.toString(countElements(input, 10));
    }

    @Override
    public String part2(List<String> input) {
        return Long.toString(countElements(input, 40));
    }

    private long countElements(final List<String> input, final int numRounds) {
        Map<String, Long> counts = new HashMap<>();
        final String word = input.get(0);
        long[] letterCounts = new long[26];
        Arrays.fill(letterCounts, 0L);

        for (int i = 0; i < word.length() - 1; i++) {
            counts.merge(word.substring(i, i+2), 1L, Long::sum);
            letterCounts[word.charAt(i) - 'A']++;
        }
        letterCounts[word.charAt(word.length() - 1) - 'A']++;

        final List<String[]> rules = input.subList(2, input.size()).stream().map(l -> l.split(" -> ")).toList();

        for (int i = 0; i < numRounds; i++) {
            Map<String, Long> newCounts = new HashMap<>();
            for (final String[] rule : rules) {
                String pair = rule[0];
                newCounts.merge(pair.charAt(0) + rule[1], counts.getOrDefault(pair, 0L), Long::sum);
                newCounts.merge(rule[1] + pair.charAt(1), counts.getOrDefault(pair, 0L), Long::sum);
                letterCounts[rule[1].charAt(0) - 'A'] += counts.getOrDefault(pair, 0L);
            }
            counts = newCounts;
        }

        long min = Long.MAX_VALUE;
        long max = Long.MIN_VALUE;
        for (final long letterCount : letterCounts) {
            if (letterCount < min && letterCount > 0)
                min = letterCount;
            if (letterCount > max)
                max = letterCount;
        }

        return max - min;
    }

}