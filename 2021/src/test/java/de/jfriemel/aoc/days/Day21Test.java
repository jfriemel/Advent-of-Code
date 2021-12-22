package de.jfriemel.aoc.days;

import de.jfriemel.aoc.Day;
import org.junit.Test;

import java.util.Arrays;
import java.util.List;

import static org.junit.Assert.assertEquals;

public class Day21Test {

    private static final Day DAY = new Day21();

    private static final String INPUT_STRING = """
            Player 1 starting position: 4
            Player 2 starting position: 8""";

    private static final List<String> INPUT = Arrays.asList(INPUT_STRING.split("\n"));

    @Test
    public void testPart1() {
        String result = DAY.part1(INPUT);
        assertEquals("739785", result);
    }

    @Test
    public void testPart2() {
        String result = DAY.part2(INPUT);
        assertEquals("444356092776315", result);
    }

}
