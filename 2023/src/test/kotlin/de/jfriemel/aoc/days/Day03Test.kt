package de.jfriemel.aoc.days

import org.junit.jupiter.api.Assertions
import org.junit.jupiter.api.Test

class Day03Test {
    private val input = """
        467..114..
        ...*......
        ..35..633.
        ......#...
        617*......
        .....+.58.
        ..592.....
        ......755.
        ...${'$'}.*....
        .664.598..
    """.trimIndent().lines()

    @Test
    fun testPart1() {
        val result = Day03.part1(input)
        Assertions.assertEquals("4361", result)
    }

    @Test
    fun testPart2() {
        val result = Day03.part2(input)
        Assertions.assertEquals("467835", result)
    }
}
