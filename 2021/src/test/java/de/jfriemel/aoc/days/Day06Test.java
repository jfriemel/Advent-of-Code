package de.jfriemel.aoc.days;

import org.junit.Test;

import java.util.Arrays;
import java.util.List;

import static org.junit.Assert.assertEquals;

public class Day06Test {

    private static final String INPUT_STRING = """
            3,4,3,1,2""";

    private static final List<String> INPUT = Arrays.asList(INPUT_STRING.split("\n"));

    @Test
    public void testPart1() {
        String result = new Day06().part1(INPUT);
        assertEquals("5934", result);
    }

    @Test
    public void testPart2() {
        String result = new Day06().part2(INPUT);
        assertEquals("26984457539", result);
    }

}
