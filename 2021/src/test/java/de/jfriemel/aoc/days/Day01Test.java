package de.jfriemel.aoc.days;

import org.junit.Test;

import java.util.Arrays;
import java.util.List;

import static org.junit.Assert.assertEquals;

public class Day01Test {

    private static final String INPUT_STRING = """
            199
            200
            208
            210
            200
            207
            240
            269
            260
            263""";

    private static final List<String> INPUT = Arrays.asList(INPUT_STRING.split("\n"));

    @Test
    public void testPart1() {
        String result = new Day01().part1(INPUT);
        assertEquals("7", result);
    }

    @Test
    public void testPart2() {
        String result = new Day01().part2(INPUT);
        assertEquals("5", result);
    }

}
