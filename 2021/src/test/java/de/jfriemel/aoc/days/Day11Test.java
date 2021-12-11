package de.jfriemel.aoc.days;

import org.junit.Test;

import java.util.Arrays;
import java.util.List;

import static org.junit.Assert.assertEquals;

public class Day11Test {

    private static final String INPUT_STRING = """
            5483143223
            2745854711
            5264556173
            6141336146
            6357385478
            4167524645
            2176841721
            6882881134
            4846848554
            5283751526""";

    private static final List<String> INPUT = Arrays.asList(INPUT_STRING.split("\n"));

    @Test
    public void testPart1() {
        String result = new Day11().part1(INPUT);
        assertEquals("1656", result);
    }

    @Test
    public void testPart2() {
        String result = new Day11().part2(INPUT);
        assertEquals("195", result);
    }

}
