package de.jfriemel.aoc.days;

import de.jfriemel.aoc.Day;

import java.util.List;

public class Day01 implements Day {

    @Override
    public String part1(final List<String> input) {
        int previous = Integer.MAX_VALUE;
        int current;
        int result = 0;
        for (final String value : input) {
            current = Integer.parseInt(value);
            if (current > previous) {
                result++;
            }
            previous = current;
        }
        return Integer.toString(result);
    }

    @Override
    public String part2(final List<String> input) {
        int a = Integer.parseInt(input.get(0));
        int b = Integer.parseInt(input.get(1));
        int c;
        int previous = Integer.MAX_VALUE;
        int current;
        int result = 0;
        for (int i = 2; i < input.size(); i++) {
            c = Integer.parseInt(input.get(i));
            current = a + b + c;
            if (current > previous) {
                result++;
            }
            a = b;
            b = c;
            previous = current;
        }
        return Integer.toString(result);
    }

}