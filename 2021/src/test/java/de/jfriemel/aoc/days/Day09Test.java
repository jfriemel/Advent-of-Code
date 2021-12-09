package de.jfriemel.aoc.days;

import org.junit.Test;

import java.util.Arrays;
import java.util.List;

import static org.junit.Assert.assertEquals;

public class Day09Test {

    private static final String INPUT_STRING = """
            2199943210
            3987894921
            9856789892
            8767896789
            9899965678""";

    private static final List<String> INPUT = Arrays.asList(INPUT_STRING.split("\n"));

    @Test
    public void testPart1() {
        String result = new Day09().part1(INPUT);
        assertEquals("15", result);
    }

    @Test
    public void testPart2() {
        String result = new Day09().part2(INPUT);
        assertEquals("1134", result);
    }

}
