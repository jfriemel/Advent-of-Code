package de.jfriemel.aoc.days

import de.jfriemel.aoc.Day

object Day14 : Day {
    override fun part1(input: List<String>): String {
        val grid = input.map { line -> line.toCharArray() }.toTypedArray()
        return tiltGrid(grid).mapIndexed { i, gridLine ->
            (grid.size - i) * gridLine.count { c -> c == 'O' }
        }.sum().toString()
    }

    override fun part2(input: List<String>): String {
        var time = 0
        val gridTimes = mutableMapOf<String, Int>()
        var currentGrid = input.map { line -> line.toCharArray() }.toTypedArray()
        while (time < 1_000_000_000) {
            currentGrid = (0..<4).fold(currentGrid) { acc, _ -> rotateGrid(tiltGrid(acc)) }
            val key = currentGrid.joinToString("") { gridLine -> gridLine.joinToString("") }
            gridTimes[key]?.let { firstTime ->
                time = ((1_000_000_000 - firstTime) / (time - firstTime)) * (time - firstTime) + firstTime
            }
            gridTimes[key] = time++
        }

        return currentGrid.mapIndexed { i, gridLine ->
            (currentGrid.size - i) * gridLine.count { c -> c == 'O' }
        }.sum().toString()
    }

    private fun tiltGrid(grid: Array<CharArray>): Array<CharArray> {
        val nextGrid = Array(grid.size) { i ->
            CharArray(grid[i].size) { j ->
                if (grid[i][j] == '#') '#' else '.'
            }
        }
        grid.forEachIndexed { i, gridLine ->
            gridLine.forEachIndexed { j, c ->
                if (c == 'O') {
                    val iFirstOccupied = (i - 1 downTo 0).firstOrNull { iNorth ->
                        nextGrid[iNorth][j] != '.'
                    } ?: -1
                    nextGrid[iFirstOccupied + 1][j] = 'O'
                }
            }
        }
        return nextGrid
    }

    private fun rotateGrid(grid: Array<CharArray>): Array<CharArray> {
        return Array(grid[0].size) { i ->
            CharArray(grid.size) { j ->
                grid[j][i]
            }.reversedArray()
        }
    }
}
