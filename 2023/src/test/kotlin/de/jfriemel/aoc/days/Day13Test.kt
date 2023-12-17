package de.jfriemel.aoc.days

import org.junit.jupiter.api.Assertions
import org.junit.jupiter.api.Test

class Day13Test {
    private val input = """
        #.##..##.
        ..#.##.#.
        ##......#
        ##......#
        ..#.##.#.
        ..##..##.
        #.#.##.#.
        
        #...##..#
        #....#..#
        ..##..###
        #####.##.
        #####.##.
        ..##..###
        #....#..#
    """.trimIndent().lines()

    @Test
    fun testPart1() {
        val result = Day13.part1(input)
        Assertions.assertEquals("405", result)
    }

    @Test
    fun testPart2() {
        val result = Day13.part2(input)
        Assertions.assertEquals("400", result)
    }
}
