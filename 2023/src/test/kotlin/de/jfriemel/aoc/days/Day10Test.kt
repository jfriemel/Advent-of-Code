package de.jfriemel.aoc.days

import org.junit.jupiter.api.Assertions
import org.junit.jupiter.api.Test

class Day10Test {
    private val input1 = """
        .....
        .S-7.
        .|.|.
        .L-J.
        .....
    """.trimIndent().lines()

    private val input2 = """
        -L|F7
        7S-7|
        L|7||
        -L-J|
        L|-JF
    """.trimIndent().lines()

    private val input3 = """
        ..F7.
        .FJ|.
        SJ.L7
        |F--J
        LJ...
    """.trimIndent().lines()

    private val input4 = """
        7-F7-
        .FJ|7
        SJLL7
        |F--J
        LJ.LJ
    """.trimIndent().lines()

    private val input5 = """
        ...........
        .S-------7.
        .|F-----7|.
        .||.....||.
        .||.....||.
        .|L-7.F-J|.
        .|..|.|..|.
        .L--J.L--J.
        ...........
    """.trimIndent().lines()

    private val input6 = """
        .F----7F7F7F7F-7....
        .|F--7||||||||FJ....
        .||.FJ||||||||L7....
        FJL7L7LJLJ||LJ.L-7..
        L--J.L7...LJS7F-7L7.
        ....F-J..F7FJ|L7L7L7
        ....L7.F7||L7|.L7L7|
        .....|FJLJ|FJ|F7|.LJ
        ....FJL-7.||.||||...
        ....L---J.LJ.LJLJ...
    """.trimIndent().lines()

    private val input7 = """
        FF7FSF7F7F7F7F7F---7
        L|LJ||||||||||||F--J
        FL-7LJLJ||||||LJL-77
        F--JF--7||LJLJ7F7FJ-
        L---JF-JLJ.||-FJLJJ7
        |F|F-JF---7F7-L7L|7|
        |FFJF7L7F-JF7|JL---7
        7-L-JL7||F7|L7F-7F7|
        L.L7LFJ|||||FJL7||LJ
        L7JLJL-JLJLJL--JLJ.L
    """.trimIndent().lines()

    @Test
    fun testPart1_1() {
        val result = Day10.part1(input1)
        Assertions.assertEquals("4", result)
    }

    @Test
    fun testPart1_2() {
        val result = Day10.part1(input2)
        Assertions.assertEquals("4", result)
    }

    @Test
    fun testPart1_3() {
        val result = Day10.part1(input3)
        Assertions.assertEquals("8", result)
    }

    @Test
    fun testPart1_4() {
        val result = Day10.part1(input4)
        Assertions.assertEquals("8", result)
    }

    @Test
    fun testPart2_1() {
        val result = Day10.part2(input5)
        Assertions.assertEquals("4", result)
    }

    @Test
    fun testPart2_2() {
        val result = Day10.part2(input6)
        Assertions.assertEquals("8", result)
    }

    @Test
    fun testPart2_3() {
        val result = Day10.part2(input7)
        Assertions.assertEquals("10", result)
    }
}
