package de.jfriemel.aoc.days;

import org.junit.Test;

import java.util.Arrays;
import java.util.List;

import static org.junit.Assert.assertEquals;

public class Day13Test {

    private static final String INPUT_STRING = """
            6,10
            0,14
            9,10
            0,3
            10,4
            4,11
            6,0
            6,12
            4,1
            0,13
            10,12
            3,4
            3,0
            8,4
            1,10
            2,14
            8,10
            9,0
                        
            fold along y=7
            fold along x=5""";

    private static final List<String> INPUT = Arrays.asList(INPUT_STRING.split("\n"));

    @Test
    public void testPart1() {
        String result = new Day13().part1(INPUT);
        assertEquals("17", result);
    }

    @Test
    public void testPart2() {
        String result = new Day13().part2(INPUT);
        assertEquals("""
                
                \u2588\u2588\u2588\u2588\u2588
                \u2588   \u2588
                \u2588   \u2588
                \u2588   \u2588
                \u2588\u2588\u2588\u2588\u2588
                    \s
                    \s""", result);
    }

}
