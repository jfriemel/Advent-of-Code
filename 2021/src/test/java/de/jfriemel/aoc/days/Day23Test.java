package de.jfriemel.aoc.days;

import de.jfriemel.aoc.Day;
import org.junit.Test;

import java.util.Arrays;
import java.util.List;

import static org.junit.Assert.assertEquals;

public class Day23Test {

    private static final Day DAY = new Day23();

    private static final String INPUT_STRING = """
            #############
            #...........#
            ###B#C#B#D###
              #A#D#C#A#
              #########""";

    private static final List<String> INPUT = Arrays.asList(INPUT_STRING.split("\n"));

    @Test
    public void testPart1() {
        String result = DAY.part1(INPUT);
        assertEquals("12521", result);
    }

    @Test
    public void testPart2() {
        String result = DAY.part2(INPUT);
        assertEquals("44169", result);
    }

}
