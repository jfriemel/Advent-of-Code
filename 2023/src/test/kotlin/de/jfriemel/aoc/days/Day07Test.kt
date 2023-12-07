package de.jfriemel.aoc.days

import org.junit.jupiter.api.Assertions
import org.junit.jupiter.api.Test

class Day07Test {
    private val input = """
        32T3K 765
        T55J5 684
        KK677 28
        KTJJT 220
        QQQJA 483
    """.trimIndent().split("\n")

    @Test
    fun testPart1() {
        val result = Day07.part1(input)
        Assertions.assertEquals("6440", result)
    }

    @Test
    fun testPart2() {
        val result = Day07.part2(input)
        Assertions.assertEquals("5905", result)
    }
}
