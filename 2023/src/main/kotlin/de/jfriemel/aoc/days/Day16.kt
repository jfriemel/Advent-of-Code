package de.jfriemel.aoc.days

import de.jfriemel.aoc.Day

object Day16 : Day {
    override fun part1(input: List<String>): String {
        val grid = input.map { line -> line.toCharArray() }.toTypedArray()
        return numEnergized(grid, Pair(0, -1), Pair(0, 1)).toString()
    }

    override fun part2(input: List<String>): String {
        val grid = input.map { line -> line.toCharArray() }.toTypedArray()
        return grid.indices.flatMap { y ->
            listOf(Pair(Pair(y, -1), Pair(0, 1)), Pair(Pair(y, grid[y].size), Pair(0, -1)))
        }.union(grid[0].indices.flatMap { x ->
            listOf(Pair(Pair(-1, x), Pair(1, 0)), Pair(Pair(grid.size, x), Pair(-1, 0)))
        }).maxOf { (coords, direction) ->
            numEnergized(grid, coords, direction)
        }.toString()
    }

    private fun numEnergized(grid: Array<CharArray>, startCoords: Pair<Int, Int>, startDirection: Pair<Int, Int>): Int {
        val stack = ArrayDeque<Pair<Pair<Int, Int>, Pair<Int, Int>>>()
        stack.add(Pair(startCoords, startDirection))
        val cache = mutableSetOf<Pair<Pair<Int, Int>, Pair<Int, Int>>>()
        while (stack.isNotEmpty()) {
            val next = stack.removeLast()
            if (next in cache) {
                continue
            }
            if (next.first != startCoords) {
                cache.add(next)
            }
            val (y, x) = next.first
            val (dy, dx) = next.second
            if (y + dy !in grid.indices || x + dx !in grid[0].indices) {
                continue
            }
            when (grid[y + dy][x + dx]) {
                '/' -> {
                    if (dy == -1) {
                        stack.addLast(Pair(Pair(y + dy, x + dx), Pair(0, 1)))
                    } else if (dy == 1) {
                        stack.addLast(Pair(Pair(y + dy, x + dx), Pair(0, -1)))
                    } else if (dx == -1) {
                        stack.addLast(Pair(Pair(y + dy, x + dx), Pair(1, 0)))
                    } else {
                        stack.addLast(Pair(Pair(y + dy, x + dx), Pair(-1, 0)))
                    }
                }

                '\\' -> {
                    if (dy == -1) {
                        stack.addLast(Pair(Pair(y + dy, x + dx), Pair(0, -1)))
                    } else if (dy == 1) {
                        stack.addLast(Pair(Pair(y + dy, x + dx), Pair(0, 1)))
                    } else if (dx == -1) {
                        stack.addLast(Pair(Pair(y + dy, x + dx), Pair(-1, 0)))
                    } else {
                        stack.addLast(Pair(Pair(y + dy, x + dx), Pair(1, 0)))
                    }
                }

                '-' -> {
                    if (dy == 0) {
                        stack.addLast(Pair(Pair(y, x + dx), Pair(dy, dx)))
                    } else {
                        stack.addLast(Pair(Pair(y + dy, x + dx), Pair(0, -1)))
                        stack.addLast(Pair(Pair(y + dy, x + dx), Pair(0, 1)))
                    }
                }

                '|' -> {
                    if (dx == 0) {
                        stack.addLast(Pair(Pair(y + dy, x), Pair(dy, dx)))
                    } else {
                        stack.addLast(Pair(Pair(y + dy, x + dx), Pair(-1, 0)))
                        stack.addLast(Pair(Pair(y + dy, x + dx), Pair(1, 0)))
                    }
                }

                else -> stack.addLast(Pair(Pair(y + dy, x + dx), Pair(dy, dx)))
            }
        }

        return cache.map { (coords, _) -> coords }.toSet().size
    }
}
