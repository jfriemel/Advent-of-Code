package de.jfriemel.aoc.days

import kotlin.test.Test
import kotlin.test.assertEquals

class Day01Test {

    private val input1 = """
        1abc2
        pqr3stu8vwx
        a1b2c3d4e5f
        treb7uchet
    """.trimIndent().split("\n")

    private val input2 = """
        two1nine
        eightwothree
        abcone2threexyz
        xtwone3four
        4nineeightseven2
        zoneight234
        7pqrstsixteen
    """.trimIndent().split("\n")

    @Test
    fun testPart1() {
        val result = Day01().part1(input1)
        assertEquals("142", result)
    }

    @Test
    fun testPart2() {
        val result = Day01().part2(input2)
        assertEquals("281", result)
    }
}
