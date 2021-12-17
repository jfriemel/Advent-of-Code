package de.jfriemel.aoc.days;

import org.junit.Test;

import java.util.Arrays;
import java.util.List;

import static org.junit.Assert.assertEquals;

public class Day17Test {

    private static final String INPUT_STRING = "target area: x=20..30, y=-10..-5";

    private static final List<String> INPUT = Arrays.asList(INPUT_STRING.split("\n"));

    @Test
    public void testPart1() {
        String result = new Day17().part1(INPUT);
        assertEquals("45", result);
    }

    @Test
    public void testPart2() {
        String result = new Day17().part2(INPUT);
        assertEquals("112", result);
    }

}
