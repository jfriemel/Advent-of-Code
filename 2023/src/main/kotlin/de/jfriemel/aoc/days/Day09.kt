package de.jfriemel.aoc.days

import de.jfriemel.aoc.Day

object Day09 : Day {
    override fun part1(input: List<String>): String {
        return sumOfPredictions(
            input.map { line -> line.split(" ").map { str -> str.toInt() }.toMutableList() }
        )
    }

    override fun part2(input: List<String>): String {
        return sumOfPredictions(
            input.map { line -> line.split(" ").map { str -> str.toInt() }.reversed().toMutableList() }
        )
    }

    private fun sumOfPredictions(sequences: List<MutableList<Int>>): String {
        return sequences.sumOf { sequence ->
            val diffs = mutableListOf(sequence)
            while (!diffs[0].all { d -> d == 0 }) {
                diffs.addFirst(diffs[0].zipWithNext { a, b -> b - a }.toMutableList())
            }
            (1..<diffs.size).forEach { i ->
                diffs[i].addLast(diffs[i].last() + diffs[i - 1].last())
            }
            diffs.last().last()
        }.toString()
    }
}
