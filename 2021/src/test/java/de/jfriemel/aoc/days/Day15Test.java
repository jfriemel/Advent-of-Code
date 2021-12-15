package de.jfriemel.aoc.days;

import org.junit.Test;

import java.util.Arrays;
import java.util.List;

import static org.junit.Assert.assertEquals;

public class Day15Test {

    private static final String INPUT_STRING = """
            1163751742
            1381373672
            2136511328
            3694931569
            7463417111
            1319128137
            1359912421
            3125421639
            1293138521
            2311944581""";

    private static final List<String> INPUT = Arrays.asList(INPUT_STRING.split("\n"));

    @Test
    public void testPart1() {
        String result = new Day15().part1(INPUT);
        assertEquals("40", result);
    }

    @Test
    public void testPart2() {
        String result = new Day15().part2(INPUT);
        assertEquals("315", result);
    }

}
