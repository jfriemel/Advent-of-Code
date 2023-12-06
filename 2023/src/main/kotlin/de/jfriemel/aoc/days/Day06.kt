package de.jfriemel.aoc.days

import de.jfriemel.aoc.Day

object Day06 : Day {
    override fun part1(input: List<String>): String {
        val races = input.map { line ->
            Regex("\\d+").findAll(line).map { matchResult -> matchResult.value.toInt() }.toList()
        }
        val winners = races[0].indices.map { raceIndex ->
            var numWinningStrats = 0
            (1..<races[0][raceIndex]).forEach { buttonTime ->
                if (buttonTime * (races[0][raceIndex] - buttonTime) > races[1][raceIndex]) {
                    numWinningStrats += 1
                }
            }
            numWinningStrats
        }
        return winners.reduce { a, b -> a * b }.toString()
    }

    override fun part2(input: List<String>): String {
        val races = input.map { line ->
            line.substringAfter(":").replace(" ", "").toLong()
        }
        var numWinningStrats = 0L
        (1..<races[0]).forEach { buttonTime ->
            if (buttonTime * (races[0] - buttonTime) > races[1]) {
                numWinningStrats += 1
            }
        }
        return numWinningStrats.toString()
    }
}
