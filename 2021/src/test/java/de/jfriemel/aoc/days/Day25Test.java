package de.jfriemel.aoc.days;

import de.jfriemel.aoc.Day;
import org.junit.Test;

import java.util.Arrays;
import java.util.List;

import static org.junit.Assert.assertEquals;

public class Day25Test {

    private static final Day DAY = new Day25();

    private static final String INPUT_STRING = """
            v...>>.vv>
            .vv>>.vv..
            >>.>v>...v
            >>v>>.>.v.
            v>v.vv.v..
            >.>>..v...
            .vv..>.>v.
            v.v..>>v.v
            ....v..v.>""";

    private static final List<String> INPUT = Arrays.asList(INPUT_STRING.split("\n"));

    @Test
    public void testPart1() {
        String result = DAY.part1(INPUT);
        assertEquals("58", result);
    }

}
