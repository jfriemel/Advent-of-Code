package de.jfriemel.aoc.days

import org.junit.jupiter.api.Assertions
import org.junit.jupiter.api.Test

class Day17Test {
    private val input1 = """
        2413432311323
        3215453535623
        3255245654254
        3446585845452
        4546657867536
        1438598798454
        4457876987766
        3637877979653
        4654967986887
        4564679986453
        1224686865563
        2546548887735
        4322674655533
    """.trimIndent().lines()

    private val input2 = """
        111111111111
        999999999991
        999999999991
        999999999991
        999999999991
    """.trimIndent().lines()

    @Test
    fun testPart1() {
        val result = Day17.part1(input1)
        Assertions.assertEquals("102", result)
    }

    @Test
    fun testPart2_1() {
        val result = Day17.part2(input1)
        Assertions.assertEquals("94", result)
    }

    @Test
    fun testPart2_2() {
        val result = Day17.part2(input2)
        Assertions.assertEquals("71", result)
    }
}
