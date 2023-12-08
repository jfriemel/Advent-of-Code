package de.jfriemel.aoc.days

import org.junit.jupiter.api.Assertions
import org.junit.jupiter.api.Test

class Day06Test {
    private val input = """
        Time:      7  15   30
        Distance:  9  40  200
    """.trimIndent().lines()

    @Test
    fun testPart1() {
        val result = Day06.part1(input)
        Assertions.assertEquals("288", result)
    }

    @Test
    fun testPart2() {
        val result = Day06.part2(input)
        Assertions.assertEquals("71503", result)
    }
}
