package de.jfriemel.aoc.days

import de.jfriemel.aoc.Day
import kotlin.math.max
import kotlin.math.min

object Day19 : Day {
    private val letterToIndex = mapOf('x' to 0, 'm' to 1, 'a' to 2, 's' to 3)

    override fun part1(input: List<String>): String {
        val (workflowsRaw, partsRaw) = input.joinToString("\n")
            .split("\n\n")
            .map { str -> str.lines() }
        val workflows = workflowsRaw.associate { workflowStr ->
            val (key, rules) = workflowStr.split("{")
            key to rules.dropLast(1)
        }
        val partsRanges = partsRaw.map { partStr ->
            Regex("\\d+").findAll(partStr).map { matchResult ->
                matchResult.value.toInt()..matchResult.value.toInt()
            }.toList().toTypedArray()
        }
        return partsRanges.filter { ranges -> evaluate(workflows, ranges, "in") == 1L }
            .sumOf { ranges -> ranges.sumOf { range -> range.first } }
            .toString()
    }

    override fun part2(input: List<String>): String {
        val workflowsRaw = input.joinToString("\n").split("\n\n")[0].lines()
        val workflows = workflowsRaw.associate { workflowStr ->
            val (key, rules) = workflowStr.split("{")
            key to rules.dropLast(1)
        }
        return evaluate(workflows, arrayOf(1..4000, 1..4000, 1..4000, 1..4000), "in").toString()
    }

    private fun evaluate(workflows: Map<String, String>, partRanges: Array<IntRange>, currentWorkflow: String): Long {
        if (currentWorkflow == "A") {
            return partRanges.fold(1L) { acc, range -> acc * range.count().toLong() }
        } else if (currentWorkflow == "R") {
            return 0L
        }
        if (partRanges.none { range -> range.isEmpty() })
            println(currentWorkflow)

        var currentRanges = partRanges
        var sum = 0L
        for (ruleStr in workflows[currentWorkflow]!!.split(",")) {
            if (!ruleStr.contains(":")) {
                return sum + evaluate(workflows, currentRanges, ruleStr)
            }

            val (checkStr, nextWorkflow) = ruleStr.split(":")
            val rateIndex = letterToIndex[checkStr[0]]!!
            val greater = checkStr[1] == '>'
            val threshold = checkStr.drop(2).toInt()

            val remainingRanges = currentRanges.clone()
            val matchingRanges = currentRanges.clone()
            if (greater) {
                remainingRanges[rateIndex] =
                    currentRanges[rateIndex].first..min(currentRanges[rateIndex].last, threshold)
                matchingRanges[rateIndex] =
                    max(currentRanges[rateIndex].first, threshold + 1)..currentRanges[rateIndex].last
            } else {
                remainingRanges[rateIndex] =
                    max(currentRanges[rateIndex].first, threshold)..currentRanges[rateIndex].last
                matchingRanges[rateIndex] =
                    currentRanges[rateIndex].first..<min(currentRanges[rateIndex].last + 1, threshold)
            }

            currentRanges = remainingRanges
            sum += evaluate(workflows, matchingRanges, nextWorkflow)
        }
        return sum
    }
}
