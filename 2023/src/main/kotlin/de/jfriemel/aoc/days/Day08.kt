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
            val limit = a * b
            var lcm = larger
            while (lcm <= limit) {
                if (lcm.mod(a) == 0L && lcm.mod(b) == 0L) {
                    return@reduce lcm
                }
                lcm += larger
            }
            lcm
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
        node: String, movements: BooleanArray,
        desertMap: Map<String, Pair<String, String>>,
        part2: Boolean,
    ): Long {
        var steps = 0L
        var current = node
        while ((!part2 && current != "ZZZ") || (part2 && !current.endsWith("Z"))) {
            current = if (movements[(steps.mod(movements.size))]) {
                desertMap[current]!!.second
            } else {
                desertMap[current]!!.first
            }
            steps += 1
        }
        return steps
    }
}
