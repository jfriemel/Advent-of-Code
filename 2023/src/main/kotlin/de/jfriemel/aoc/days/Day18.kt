package de.jfriemel.aoc.days

import de.jfriemel.aoc.Day
import kotlin.math.abs

object Day18 : Day {
    override fun part1(input: List<String>): String {
        val instrToDir = mapOf("U" to Pair(1, 0), "D" to Pair(-1, 0), "L" to Pair(0, -1), "R" to Pair(0, 1))
        return computeArea(
            input.map { line ->
                val (instr, length, _) = line.split(" ")
                Pair(instrToDir[instr]!!, length.toInt())
            }
        )
    }

    override fun part2(input: List<String>): String {
        val instrToDir = mapOf('0' to Pair(0, 1), '1' to Pair(-1, 0), '2' to Pair(0, -1), '3' to Pair(1, 0))
        return computeArea(
            input.map { line ->
                val hex = line.substring(line.length - 7..line.length - 3)
                val dir = instrToDir[line[line.length - 2]]!!
                Pair(dir, hex.toInt(radix = 16))
            }
        )
    }

    private fun computeArea(instructions: List<Pair<Pair<Int, Int>, Int>>): String {
        val (_, _, area, perimeter) =
            instructions.fold(listOf(0L, 0L, 0L, 0L)) { (y, x, area, perimeter), (dir, length) ->
                val (nextY, nextX) = Pair(y + length * dir.first, x + length * dir.second)
                listOf(nextY, nextX, area + y * nextX - nextY * x, perimeter + length)
            }
        return (abs(area / 2L) + perimeter / 2L + 1L).toString()
    }
}
