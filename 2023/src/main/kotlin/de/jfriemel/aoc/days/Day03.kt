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

        var sum = 0
        locations.forEachIndexed { digitY, matchResultLists ->
            matchResultLists.forEach { matchResult ->
                val valid = matchResult.range.any { digitX ->
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
                if (valid) {
                    sum += matchResult.value.toInt()
                }
            }
        }
        return sum.toString()
    }

    override fun part2(input: List<String>): String {
        val numLocations: List<Pair<Int, MatchResult>> = mutableListOf()
        input.forEachIndexed { numX, line ->
            Regex("\\d+").findAll(line).forEach { matchResult ->
                numLocations.addLast(Pair(numX, matchResult))
            }
        }

        var sum = 0
        input.forEachIndexed { gearY, line ->
            Regex("\\*").findAll(line).map { match -> match.range.first }.forEach { gearX ->
                val neighbors: List<Int> = mutableListOf()
                numLocations.forEach { (numY, numMatch) ->
                    val valid = arrayOf(
                        Pair(-1, -1),
                        Pair(-1, 0),
                        Pair(-1, 1),
                        Pair(0, -1),
                        Pair(0, 1),
                        Pair(1, -1),
                        Pair(1, 0),
                        Pair(1, 1),
                    ).any { (offsetX, offsetY) ->
                        gearX + offsetX in numMatch.range && gearY + offsetY == numY
                    }
                    if (valid) {
                        neighbors.addLast(numMatch.value.toInt())
                    }
                }
                if (neighbors.size == 2) {
                    sum += neighbors[0] * neighbors[1]
                }
            }
        }
        return sum.toString()
    }
}
