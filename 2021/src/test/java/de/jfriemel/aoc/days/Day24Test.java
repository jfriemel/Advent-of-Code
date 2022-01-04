package de.jfriemel.aoc.days;

import org.junit.Test;

import static org.junit.Assert.assertEquals;

public class Day24Test {

    private static final Day24 DAY = new Day24();

    // Since there is no test case in the problem description, this test uses my puzzle input.
    private static final int[] ADD_X = {14, 15, 13, -10, 14, -3, -14, 12, 14, 12, -6, -6, -2, -9};
    private static final int[] ADD_Y = {8, 11, 2, 11, 1, 5, 10, 6, 1, 11, 9, 14, 11, 2};
    private static final int[] DIV_Z = {1, 1, 1, 26, 1, 26, 26, 1, 1, 1, 26, 26, 26, 26};

    @Test
    public void testPart1() {
        long result = DAY.solveConstraints(ADD_X, ADD_Y, DIV_Z, true);
        assertEquals(99919765949498L, result);
    }

    @Test
    public void testPart2() {
        long result = DAY.solveConstraints(ADD_X, ADD_Y, DIV_Z, false);
        assertEquals(24913111616151L, result);
    }

}
