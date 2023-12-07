package de.jfriemel.aoc.days

import de.jfriemel.aoc.Day
import kotlin.math.max
import kotlin.math.min

object Day05 : Day {
    override fun part1(input: List<String>): String {
        val inputParts = input.joinToString("\n")
            .split("\n\n")
            .map { block -> block.split("\n") }
        val seedRanges = inputParts[0][0].substringAfter("seeds: ")
            .split(" ")
            .map { str -> str.toLong()..<(str.toLong() + 1) }

        return getMinLocation(seedRanges, inputParts)
    }

    override fun part2(input: List<String>): String {
        val inputParts = input.joinToString("\n")
            .split("\n\n")
            .map { block -> block.split("\n") }
        val seedRanges = inputParts[0][0].substringAfter("seeds: ")
            .split(" ")
            .map { str -> str.toLong() }
            .chunked(2)
            .map { (rangeStart, rangeLength) -> rangeStart..<(rangeStart + rangeLength) }

        return getMinLocation(seedRanges, inputParts)
    }

    private fun getMinLocation(seedRanges: List<LongRange>, inputParts: List<List<String>>): String {
        val mapRanges: Array<List<Triple<Long, Long, Long>>> = Array(inputParts.size - 1) { mutableListOf() }
        (1..<inputParts.size).forEach { i ->
            (1..<inputParts[i].size).forEach { j ->
                val (destStart, seedStart, length) = inputParts[i][j].split(" ").map { str -> str.toLong() }
                mapRanges[i - 1].addLast(Triple(destStart, seedStart, length))
            }
        }

        var currentRanges = seedRanges
        mapRanges.indices.forEach { level ->
            currentRanges = getNextRanges(currentRanges, level, mapRanges)
        }
        return currentRanges.minOf { range -> range.first }.toString()
    }

    private fun getNextRanges(
        seedRanges: List<LongRange>,
        level: Int,
        mapRanges: Array<List<Triple<Long, Long, Long>>>,
    ): List<LongRange> {
        var remaining = seedRanges
        val result = mutableListOf<LongRange>()

        mapRanges[level].forEach { (destStart, seedStart, length) ->
            val nextRemaining = mutableListOf<LongRange>()
            val destOffset = destStart - seedStart
            remaining.forEach { seedRange ->
                val intersect = max(seedStart, seedRange.first)..min(seedStart + length - 1, seedRange.last)
                if (intersect.isEmpty()) {
                    nextRemaining.addLast(seedRange)
                } else {
                    result.addLast((destOffset + intersect.first)..(destOffset + intersect.last))
                    nextRemaining.add(seedRange.first..<intersect.first) // Left of intersection
                    nextRemaining.add((intersect.last + 1)..seedRange.last) // Right of intersection
                }
            }
            remaining = nextRemaining.filter { range -> !range.isEmpty() }
        }

        result.addAll(remaining)
        return result
    }
}
