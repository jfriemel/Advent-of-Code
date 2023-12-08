package de.jfriemel.aoc.days

import org.junit.jupiter.api.Assertions
import org.junit.jupiter.api.Test

class Day08Test {
    private val input1 = """
        RL
        
        AAA = (BBB, CCC)
        BBB = (DDD, EEE)
        CCC = (ZZZ, GGG)
        DDD = (DDD, DDD)
        EEE = (EEE, EEE)
        GGG = (GGG, GGG)
        ZZZ = (ZZZ, ZZZ)
    """.trimIndent().lines()

    private val input2 = """
        LLR

        AAA = (BBB, BBB)
        BBB = (AAA, ZZZ)
        ZZZ = (ZZZ, ZZZ)
    """.trimIndent().lines()

    private val input3 = """
        LR

        11A = (11B, XXX)
        11B = (XXX, 11Z)
        11Z = (11B, XXX)
        22A = (22B, XXX)
        22B = (22C, 22C)
        22C = (22Z, 22Z)
        22Z = (22B, 22B)
        XXX = (XXX, XXX)
    """.trimIndent().lines()

    @Test
    fun testPart1_1() {
        val result = Day08.part1(input1)
        Assertions.assertEquals("2", result)
    }

    @Test
    fun testPart1_2() {
        val result = Day08.part1(input2)
        Assertions.assertEquals("6", result)
    }

    @Test
    fun testPart2() {
        val result = Day08.part2(input3)
        Assertions.assertEquals("6", result)
    }
}
