package de.jfriemel.aoc.days

import org.junit.jupiter.api.Assertions
import org.junit.jupiter.api.Test

class Day09Test {
    private val input = """
        0 3 6 9 12 15
        1 3 6 10 15 21
        10 13 16 21 30 45
    """.trimIndent().lines()

    @Test
    fun testPart1() {
        val result = Day09.part1(input)
        Assertions.assertEquals("114", result)
    }

    @Test
    fun testPart2() {
        val result = Day09.part2(input)
        Assertions.assertEquals("2", result)
    }
}
