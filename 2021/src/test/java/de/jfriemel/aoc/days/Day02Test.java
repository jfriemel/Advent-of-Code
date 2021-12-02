package de.jfriemel.aoc.days;

import org.junit.Test;

import java.util.Arrays;
import java.util.List;

import static org.junit.Assert.assertEquals;

public class Day02Test {

    private static final String INPUT_STRING = """
            forward 5
            down 5
            forward 8
            up 3
            down 8
            forward 2""";

    private static final List<String> INPUT = Arrays.asList(INPUT_STRING.split("\n"));

    @Test
    public void testPart1() {
        String result = new Day02().part1(INPUT);
        assertEquals("150", result);
    }

    @Test
    public void testPart2() {
        String result = new Day02().part2(INPUT);
        assertEquals("900", result);
    }

}
