package de.jfriemel.aoc.days;

import org.junit.Test;

import java.util.Arrays;
import java.util.LinkedList;
import java.util.List;

import static org.junit.Assert.*;

public class Day18Test {

    private static final Day18 DAY_18 = new Day18();

    private static final String INPUT_STRING = """
            [[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]]
            [[[5,[2,8]],4],[5,[[9,9],0]]]
            [6,[[[6,2],[5,6]],[[7,6],[4,7]]]]
            [[[6,[0,7]],[0,9]],[4,[9,[9,0]]]]
            [[[7,[6,4]],[3,[1,3]]],[[[5,5],1],9]]
            [[6,[[7,3],[3,2]]],[[[3,8],[5,7]],4]]
            [[[[5,4],[7,7]],8],[[8,3],8]]
            [[9,3],[[9,9],[6,[4,9]]]]
            [[2,[[7,7],7]],[[5,8],[[9,3],[0,2]]]]
            [[[[5,2],5],[8,[3,7]]],[[5,[7,5]],[4,4]]]""";

    private static final List<String> INPUT = Arrays.asList(INPUT_STRING.split("\n"));

    @Test
    public void testParse() {
        final String inputParse = """
            [1,2]
            [[1,2],3]
            [9,[8,7]]
            [[1,9],[8,5]]
            [[[[1,2],[3,4]],[[5,6],[7,8]]],9]
            [[[9,[3,8]],[[0,9],6]],[[[3,7],[4,9]],3]]
            [[[[1,3],[5,3]],[[1,3],[8,7]]],[[[4,9],[6,9]],[[8,2],[7,3]]]]""";
        for (final String snailString : inputParse.split("\n")) {
            assertEquals(snailString, DAY_18.parseSnailfish(snailString).toString());
        }
    }

    @Test
    public void testMagnitude() {
        final String inputStringMagnitude = """
            [[9,1],[1,9]]=129
            [[1,2],[[3,4],5]]=143
            [[[[0,7],4],[[7,8],[6,0]]],[8,1]]=1384
            [[[[1,1],[2,2]],[3,3]],[4,4]]=445
            [[[[3,0],[5,3]],[4,4]],[5,5]]=791
            [[[[5,0],[7,4]],[5,5]],[6,6]]=1137
            [[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]=3488""";
        for (final String snailStringResult : inputStringMagnitude.split("\n")) {
            final String[] parts = snailStringResult.split("=");
            final Snailfish snailfish = DAY_18.parseSnailfish(parts[0]);
            assertEquals(parts[0], snailfish.toString());
            assertEquals(Long.parseLong(parts[1]), DAY_18.calcMagnitude(snailfish));
        }
    }

    @Test
    public void testSplit() {
        Snailfish snailfish = DAY_18.parseSnailfish("[[[[0,7],4],[15,[0,13]]],[1,1]]");
        assertEquals("[[[[0,7],4],[15,[0,13]]],[1,1]]", snailfish.toString());
        assertTrue(DAY_18.split(snailfish));
        assertEquals("[[[[0,7],4],[[7,8],[0,13]]],[1,1]]", snailfish.toString());
        assertTrue(DAY_18.split(snailfish));
        assertEquals("[[[[0,7],4],[[7,8],[0,[6,7]]]],[1,1]]", snailfish.toString());
        assertFalse(DAY_18.split(snailfish));
    }

    @Test
    public void testExplode() {
        final String inputStringExplode = """
                [[[[[9,8],1],2],3],4]=[[[[0,9],2],3],4]
                [7,[6,[5,[4,[3,2]]]]]=[7,[6,[5,[7,0]]]]
                [[6,[5,[4,[3,2]]]],1]=[[6,[5,[7,0]]],3]
                [[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]=[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]
                """;
        for (final String snailStringResult : inputStringExplode.split("\n")) {
            final String[] parts = snailStringResult.split("=");
            final Snailfish snailfish = DAY_18.parseSnailfish(parts[0]);
            assertEquals(parts[0], snailfish.toString());
            DAY_18.explode(snailfish, 0, new LinkedList<>());
            assertEquals(parts[1], snailfish.toString());
        }
    }

    @Test
    public void testAddReduce() {
        final Snailfish snail1 = DAY_18.parseSnailfish("[[[[4,3],4],4],[7,[[8,4],9]]]");
        final Snailfish snail2 = DAY_18.parseSnailfish("[1,1]");
        final Snailfish snailfish = DAY_18.add(snail1, snail2);

        assertEquals("[[[[4,3],4],4],[7,[[8,4],9]]]", snail1.toString());
        assertEquals("[1,1]", snail2.toString());
        assertEquals("[[[[[4,3],4],4],[7,[[8,4],9]]],[1,1]]", snailfish.toString());

        DAY_18.reduce(snailfish);
        assertEquals("[[[[0,7],4],[[7,8],[6,0]]],[8,1]]", snailfish.toString());
    }

    @Test
    public void testAddReduceBig() {
        final String inputAddReduce = """
                [7,[[[3,7],[4,3]],[[6,3],[8,8]]]]
                [[2,[[0,8],[3,4]]],[[[6,7],1],[7,[1,6]]]]
                [[[[2,4],7],[6,[0,5]]],[[[6,8],[2,8]],[[2,1],[4,5]]]]
                [7,[5,[[3,8],[1,4]]]]
                [[2,[2,2]],[8,[8,1]]]
                [2,9]
                [1,[[[9,3],9],[[9,0],[0,7]]]]
                [[[5,[7,4]],7],1]
                [[[[4,2],2],6],[8,7]]""";
        Snailfish snailfish = DAY_18.parseSnailfish("[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]");
        assertEquals("[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]", snailfish.toString());

        for (final String snailString : inputAddReduce.split("\n")) {
            final Snailfish snail2 = DAY_18.parseSnailfish(snailString);
            assertEquals(snailString, snail2.toString());
            snailfish = DAY_18.add(snailfish, snail2);
            DAY_18.reduce(snailfish);
        }
        assertEquals("[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]", snailfish.toString());
    }

    @Test
    public void testPart1() {
        assertEquals("4140", DAY_18.part1(INPUT));
    }

    @Test
    public void testPart2() {
        assertEquals("3993", DAY_18.part2(INPUT));
    }

}
