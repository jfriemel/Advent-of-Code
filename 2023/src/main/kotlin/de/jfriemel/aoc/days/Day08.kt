package de.jfriemel.aoc.days

import de.jfriemel.aoc.Day

object Day08 : Day {
    override fun part1(input: List<String>): String {
        val (movements, desertMap) = parseInput(input)
        return numSteps("AAA", movements, desertMap, false).toString()
    }

    override fun part2(input: List<String>): String {
        val (movements, desertMap) = parseInput(input)
        return desertMap.keys.filter { node -> node.endsWith("A") }.map { node ->
            numSteps(node, movements, desertMap, true)
        }.reduce { a, b ->
            val larger = if (a > b) a else b
            (larger..(a * b) step larger).first { candidate -> candidate.mod(a) == 0L && candidate.mod(b) == 0L }
        }.toString()
    }

    private fun parseInput(input: List<String>): Pair<BooleanArray, Map<String, Pair<String, String>>> {
        val movements = input[0].map { c -> c == 'R' }.toBooleanArray()
        val desertMap = input.subList(2, input.size).associate { line ->
            line.substring(0..2) to Pair(line.substring(7..9), line.substring(12..14))
        }
        return Pair(movements, desertMap)
    }

    private fun numSteps(
        startNode: String,
        movements: BooleanArray,
        desertMap: Map<String, Pair<String, String>>,
        part2: Boolean,
    ): Long {
        return generateSequence(Pair(startNode, 0L)) { (node, steps) ->
            Pair(
                if (movements[(steps.mod(movements.size))]) desertMap[node]!!.second else desertMap[node]!!.first,
                steps + 1,
            )
        }.first { (node, _) -> (!part2 && node == "ZZZ") || (part2 && node.endsWith("Z")) }.second
    }
}
