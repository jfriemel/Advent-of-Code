package de.jfriemel.aoc.days

import org.junit.jupiter.api.Assertions
import org.junit.jupiter.api.Test

class Day12Test {
    private val input = """
        ???.### 1,1,3
        .??..??...?##. 1,1,3
        ?#?#?#?#?#?#?#? 1,3,1,6
        ????.#...#... 4,1,1
        ????.######..#####. 1,6,5
        ?###???????? 3,2,1
    """.trimIndent().lines()

    @Test
    fun testPart1() {
        val result = Day12.part1(input)
        Assertions.assertEquals("21", result)
    }

    @Test
    fun testPart2() {
        val result = Day12.part2(input)
        Assertions.assertEquals("525152", result)
    }
}
