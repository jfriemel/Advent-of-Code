package de.jfriemel.aoc.days

import de.jfriemel.aoc.Day
import java.util.PriorityQueue

object Day17 : Day {
    override fun part1(input: List<String>) = shortestPath(input, false)

    override fun part2(input: List<String>) = shortestPath(input, true)

    private fun shortestPath(input: List<String>, part2: Boolean): String {
        val grid = input.map { line -> line.toCharArray().map { c -> c.digitToInt() }.toIntArray() }.toTypedArray()
        val dirToOffset = mapOf(0 to Pair(-1, 0), 1 to Pair(0, 1), 2 to Pair(1, 0), 3 to Pair(0, -1))
        val maxForward = if (part2) 9 else 2

        val (destY, destX) = Pair(grid.size - 1, grid[0].size - 1)
        val visited = mutableSetOf<List<Int>>()
        val distances = mutableMapOf<List<Int>, Int>()
        val adjacent = PriorityQueue<Pair<List<Int>, Int>>(compareBy { (_, dist) -> dist })
        adjacent.add(Pair(listOf(1, 0, 2, maxForward), grid[1][0]))
        adjacent.add(Pair(listOf(0, 1, 1, maxForward), grid[0][1]))
        while (true) {
            val (current, curDist) = adjacent.remove()
            if (current in visited) {
                continue
            }

            visited.add(current)
            distances[current] = curDist
            val (curY, curX, curDir, curStreak) = current
            if (curY == destY && curX == destX && curStreak <= 6) {
                return curDist.toString()
            }

            adjacent.addAll(
                listOf(curDir, (curDir + 1).mod(4), (curDir - 1).mod(4)).mapNotNull { dir ->
                    val (dy, dx) = dirToOffset[dir]!!
                    if (curY + dy in grid.indices && curX + dx in grid[0].indices) {
                        if (dir == curDir && curStreak > 0) {
                            listOf(curY + dy, curX + dx, dir, curStreak - 1)
                        } else if (dir != curDir && (!part2 || curStreak <= 6)) {
                            listOf(curY + dy, curX + dx, dir, maxForward)
                        } else {
                            null
                        }
                    } else {
                        null
                    }
                }.map { nbr ->
                    val (nbrY, nbrX, _, _) = nbr
                    Pair(nbr, curDist + grid[nbrY][nbrX])
                }
            )
        }
    }
}
