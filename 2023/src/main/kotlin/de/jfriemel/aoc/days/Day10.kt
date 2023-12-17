package de.jfriemel.aoc.days

import de.jfriemel.aoc.Day
import kotlin.math.abs

object Day10 : Day {
    override fun part1(input: List<String>) = (traverse(input).size / 2).toString()

    override fun part2(input: List<String>): String {
        val path = traverse(input)
        val area = abs(
            path.indices.sumOf { i ->
                val (y1, x1) = path[i]
                val (y2, x2) = path[(i + 1) % path.size]
                y1 * x2 - y2 * x1
            } / 2
        )
        return (area - path.size / 2 + 1).toString()
    }

    private fun traverse(input: List<String>): List<Pair<Int, Int>> {
        lateinit var start: Pair<Int, Int>
        val pipeMap = input.mapIndexed { i, line ->
            line.mapIndexed { j, c ->
                when (c) {
                    '|' -> booleanArrayOf(true, false, true, false)
                    '-' -> booleanArrayOf(false, true, false, true)
                    'L' -> booleanArrayOf(true, true, false, false)
                    'J' -> booleanArrayOf(true, false, false, true)
                    '7' -> booleanArrayOf(false, false, true, true)
                    'F' -> booleanArrayOf(false, true, true, false)
                    'S' -> {
                        start = Pair(i, j)
                        booleanArrayOf(
                            input.getOrElse(i - 1) { ".".repeat(line.length) }[j] in charArrayOf('|', '7', 'F'),
                            input[i].getOrElse(j + 1) { '.' } in charArrayOf('-', 'J', '7'),
                            input.getOrElse(i + 1) { ".".repeat(line.length) }[j] in charArrayOf('|', 'L', 'J'),
                            input[i].getOrElse(j - 1) { '.' } in charArrayOf('-', 'L', 'F'),
                        )
                    }
                    else -> booleanArrayOf(false, false, false, false)
                }
            }.toTypedArray()
        }.toTypedArray()

        var current = start
        var direction = 0
        val path = mutableListOf<Pair<Int, Int>>()
        while (current != start || path.isEmpty()) {
            path.addLast(current)
            direction = (direction + (-1..1).first { offset ->
                pipeMap[current.first][current.second][(direction + offset).mod(4)]
            }).mod(4)
            current = when (direction) {
                0 -> Pair(current.first - 1, current.second)
                1 -> Pair(current.first, current.second + 1)
                2 -> Pair(current.first + 1, current.second)
                3 -> Pair(current.first, current.second - 1)
                else -> current
            }
        }
        return path
    }
}
