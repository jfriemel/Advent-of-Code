package de.jfriemel.aoc.days

import de.jfriemel.aoc.Day

object Day06 : Day {
    override fun part1(input: List<String>): String {
        val races = input.map { line ->
            Regex("\\d+").findAll(line).map { matchResult -> matchResult.value.toInt() }.toList()
        }
        return races[0].indices.map { raceIndex ->
            (1..<races[0][raceIndex]).count { buttonTime ->
                buttonTime * (races[0][raceIndex] - buttonTime) > races[1][raceIndex]
            }
        }.reduce { a, b -> a * b }.toString()
    }

    override fun part2(input: List<String>): String {
        val races = input.map { line ->
            line.substringAfter(":").replace(" ", "").toLong()
        }
        return (1..<races[0]).count { buttonTime ->
            (buttonTime * (races[0] - buttonTime)) > races[1]
        }.toString()
    }
}
