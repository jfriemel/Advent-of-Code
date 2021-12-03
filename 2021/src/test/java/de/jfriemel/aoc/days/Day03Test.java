package de.jfriemel.aoc.days;

import org.junit.Test;

import java.util.Arrays;
import java.util.List;

import static org.junit.Assert.assertEquals;

public class Day03Test {

    private static final String INPUT_STRING = """
            00100
            11110
            10110
            10111
            10101
            01111
            00111
            11100
            10000
            11001
            00010
            01010""";

    private static final List<String> INPUT = Arrays.asList(INPUT_STRING.split("\n"));

    @Test
    public void testPart1() {
        String result = new Day03().part1(INPUT);
        assertEquals("198", result);
    }

    @Test
    public void testPart2() {
        String result = new Day03().part2(INPUT);
        assertEquals("230", result);
    }

}
