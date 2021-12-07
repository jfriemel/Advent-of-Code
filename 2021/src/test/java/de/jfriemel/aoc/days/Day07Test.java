package de.jfriemel.aoc.days;

import org.junit.Test;

import java.util.Arrays;
import java.util.List;

import static org.junit.Assert.assertEquals;

public class Day07Test {

    private static final String INPUT_STRING = """
            16,1,2,0,4,2,7,1,2,14""";

    private static final List<String> INPUT = Arrays.asList(INPUT_STRING.split("\n"));

    @Test
    public void testPart1() {
        String result = new Day07().part1(INPUT);
        assertEquals("37", result);
    }

    @Test
    public void testPart2() {
        String result = new Day07().part2(INPUT);
        assertEquals("168", result);
    }

}
