package de.jfriemel.aoc.days

import de.jfriemel.aoc.Day

object Day11 : Day {
    override fun part1(input: List<String>) = sumOfGalaxyDistances(input, 1L)

    override fun part2(input: List<String>) = sumOfGalaxyDistances(input, 999_999L)

    private fun sumOfGalaxyDistances(input: List<String>, expansionFactor: Long): String {
        val galaxies = input.flatMapIndexed { y, line ->
            Regex("#").findAll(line).map { xMatch -> Pair(xMatch.range.first, y) }
        }
        val galaxyPairs = galaxies.flatMapIndexed { index, galaxy ->
            List(galaxies.size) { galaxy }.zip(galaxies.subList(index + 1, galaxies.size))
        }
        val expandCols = input[0].indices.filter { x -> input.indices.none { y -> input[y][x] == '#' } }
        val expandRows = input.indices.filter { y -> input[y].indices.none { x -> input[y][x] == '#' } }
        return galaxyPairs.sumOf { (galaxy1, galaxy2) ->
            val (x1, x2) = intArrayOf(galaxy1.first, galaxy2.first).sorted()
            val (y1, y2) = intArrayOf(galaxy1.second, galaxy2.second).sorted()
            x2 - x1 + y2 - y1 +
                expansionFactor * expandCols.count { x -> x in x1..<x2 } +
                expansionFactor * expandRows.count { y -> y in y1..<y2 }
        }.toString()
    }
}
