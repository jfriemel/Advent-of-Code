package de.jfriemel.aoc.days

import de.jfriemel.aoc.Day

object Day03 : Day {
    override fun part1(input: List<String>): String {
        val chars = input.map { str -> str.toCharArray() }.toTypedArray()
        val nonSymbols = ".0123456789".toCharArray()
        val locations: List<List<MatchResult>> = arrayListOf()
        input.forEach { line ->
            locations.addLast(Regex("\\d+").findAll(line).toList())
        }

        return locations.foldIndexed(0) { digitY, acc, matchResultLists ->
            acc + matchResultLists.filter { matchResult ->
                matchResult.range.any { digitX ->
                    arrayOf(
                        Pair(-1, -1),
                        Pair(-1, 0),
                        Pair(-1, 1),
                        Pair(0, -1),
                        Pair(0, 1),
                        Pair(1, -1),
                        Pair(1, 0),
                        Pair(1, 1),
                    ).any { (offsetX, offsetY) ->
                        chars.getOrElse(digitY + offsetY) { "".toCharArray() }
                            .getOrElse(digitX + offsetX) { '.' } !in nonSymbols
                    }
                }
            }.sumOf { matchResult -> matchResult.value.toInt() }
        }.toString()
    }

    override fun part2(input: List<String>): String {
        val numLocations: List<Pair<Int, MatchResult>> = mutableListOf()
        input.forEachIndexed { numX, line ->
            Regex("\\d+").findAll(line).forEach { matchResult ->
                numLocations.addLast(Pair(numX, matchResult))
            }
        }

        return input.foldIndexed(0) { gearY, acc, line ->
            acc + Regex("\\*").findAll(line).map { match -> match.range.first }.map { gearX ->
                numLocations.filter { (numY, numMatch) ->
                    arrayOf(
                        Pair(-1, -1),
                        Pair(-1, 0),
                        Pair(-1, 1),
                        Pair(0, -1),
                        Pair(0, 1),
                        Pair(1, -1),
                        Pair(1, 0),
                        Pair(1, 1),
                    ).any { (offsetX, offsetY) -> gearX + offsetX in numMatch.range && gearY + offsetY == numY }
                }.map { (_, numMatch) -> numMatch.value.toInt() }
            }.filter { neighbors -> neighbors.size == 2 }
                .sumOf { neighbors -> neighbors[0] * neighbors[1] }
        }.toString()
    }
}
