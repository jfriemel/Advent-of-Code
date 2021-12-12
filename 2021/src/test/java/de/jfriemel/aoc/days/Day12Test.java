package de.jfriemel.aoc.days;

import org.junit.Test;

import java.util.Arrays;
import java.util.List;

import static org.junit.Assert.assertEquals;

public class Day12Test {

    private static final String INPUT_STRING_A = """
            start-A
            start-b
            A-c
            A-b
            b-d
            A-end
            b-end""";

    private static final String INPUT_STRING_B = """
            dc-end
            HN-start
            start-kj
            dc-start
            dc-HN
            LN-dc
            HN-end
            kj-sa
            kj-HN
            kj-dc""";

    private static final String INPUT_STRING_C = """
            fs-end
            he-DX
            fs-he
            start-DX
            pj-DX
            end-zg
            zg-sl
            zg-pj
            pj-he
            RW-he
            fs-DX
            pj-RW
            zg-RW
            start-pj
            he-WI
            zg-he
            pj-fs
            start-RW""";

    private static final List<String> INPUT_A = Arrays.asList(INPUT_STRING_A.split("\n"));
    private static final List<String> INPUT_B = Arrays.asList(INPUT_STRING_B.split("\n"));
    private static final List<String> INPUT_C = Arrays.asList(INPUT_STRING_C.split("\n"));

    @Test
    public void testPart1_a() {
        String result = new Day12().part1(INPUT_A);
        assertEquals("10", result);
    }

    @Test
    public void testPart1_b() {
        String result = new Day12().part1(INPUT_B);
        assertEquals("19", result);
    }

    @Test
    public void testPart1_c() {
        String result = new Day12().part1(INPUT_C);
        assertEquals("226", result);
    }

    @Test
    public void testPart2_a() {
        String result = new Day12().part2(INPUT_A);
        assertEquals("36", result);
    }

    @Test
    public void testPart2_b() {
        String result = new Day12().part2(INPUT_B);
        assertEquals("103", result);
    }

    @Test
    public void testPart2_c() {
        String result = new Day12().part2(INPUT_C);
        assertEquals("3509", result);
    }

}
