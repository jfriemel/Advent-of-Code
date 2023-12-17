package de.jfriemel.aoc.days

import org.junit.jupiter.api.Assertions
import org.junit.jupiter.api.Test

class Day14Test {
    private val input = """
        O....#....
        O.OO#....#
        .....##...
        OO.#O....O
        .O.....O#.
        O.#..O.#.#
        ..O..#O..O
        .......O..
        #....###..
        #OO..#....
    """.trimIndent().lines()

    @Test
    fun testPart1() {
        val result = Day14.part1(input)
        Assertions.assertEquals("136", result)
    }

    @Test
    fun testPart2() {
        val result = Day14.part2(input)
        Assertions.assertEquals("64", result)
    }
}
