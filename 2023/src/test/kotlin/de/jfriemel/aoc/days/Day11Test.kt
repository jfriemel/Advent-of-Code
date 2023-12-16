package de.jfriemel.aoc.days

import org.junit.jupiter.api.Assertions
import org.junit.jupiter.api.Test

class Day11Test {
    private val input = """
        ...#......
        .......#..
        #.........
        ..........
        ......#...
        .#........
        .........#
        ..........
        .......#..
        #...#.....
    """.trimIndent().lines()

    @Test
    fun testPart1() {
        val result = Day11.part1(input)
        Assertions.assertEquals("374", result)
    }

    @Test
    fun testPart2() {
        val result = Day11.part2(input)
        Assertions.assertEquals("82000210", result)
    }
}
