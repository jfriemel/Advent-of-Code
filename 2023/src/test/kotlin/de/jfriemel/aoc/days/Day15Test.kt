package de.jfriemel.aoc.days

import org.junit.jupiter.api.Assertions
import org.junit.jupiter.api.Test

class Day15Test {
    private val input = "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7".lines()

    @Test
    fun testPart1() {
        val result = Day15.part1(input)
        Assertions.assertEquals("1320", result)
    }

    @Test
    fun testPart2() {
        val result = Day15.part2(input)
        Assertions.assertEquals("145", result)
    }
}
