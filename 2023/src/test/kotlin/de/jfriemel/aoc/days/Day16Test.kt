package de.jfriemel.aoc.days

import org.junit.jupiter.api.Assertions
import org.junit.jupiter.api.Test

class Day16Test {
    private val input = """
        .|...\....
        |.-.\.....
        .....|-...
        ........|.
        ..........
        .........\
        ..../.\\..
        .-.-/..|..
        .|....-|.\
        ..//.|....
    """.trimIndent().lines()

    @Test
    fun testPart1() {
        val result = Day16.part1(input)
        Assertions.assertEquals("46", result)
    }

    @Test
    fun testPart2() {
        val result = Day16.part2(input)
        Assertions.assertEquals("51", result)
    }
}
