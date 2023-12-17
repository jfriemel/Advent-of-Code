package de.jfriemel.aoc.days

import de.jfriemel.aoc.Day
import kotlin.math.min

object Day13 : Day {
    override fun part1(input: List<String>): String {
        return input.joinToString("\n").split("\n\n").map { block ->
            block.lines().map { line -> line.toCharArray() }.toTypedArray()
        }.sumOf { grid ->
            100 * mirrorIndex(grid) + mirrorIndex(rotateGrid(grid))
        }.toString()
    }

    override fun part2(input: List<String>): String {
        return input.joinToString("\n").split("\n\n").map { block ->
            block.lines().map { line -> line.toCharArray() }.toTypedArray()
        }.sumOf { grid ->
            val hOrig = mirrorIndex(grid)
            val vOrig = mirrorIndex(rotateGrid(grid))
            smudges(grid).map { candidate ->
                100 * mirrorIndex(candidate, hOrig) + mirrorIndex(rotateGrid(candidate), vOrig)
            }.first { value -> value > 0 }
        }.toString()
    }

    private fun mirrorIndex(grid: Array<CharArray>, ignore: Int? = null): Int {
        return (1..<grid.size).firstOrNull { hIndex ->
            hIndex != ignore && (0..<min(hIndex, grid.size - hIndex)).all { mOffset ->
                grid[hIndex - 1 - mOffset].contentEquals(grid[hIndex + mOffset])
            }
        } ?: 0
    }

    private fun rotateGrid(grid: Array<CharArray>): Array<CharArray> {
        return Array(grid[0].size) { i ->
            CharArray(grid.size) { j ->
                grid[j][i]
            }
        }
    }

    private fun smudges(grid: Array<CharArray>): List<Array<CharArray>> {
        return grid.indices.flatMap { i ->
            grid[i].indices.map { j ->
                val sGrid = grid.map { line -> line.clone() }.toTypedArray()
                sGrid[i][j] = if (grid[i][j] == '.') '#' else '.'
                sGrid
            }
        }
    }
}
