package de.jfriemel.aoc.days

import de.jfriemel.aoc.Day
import kotlin.math.floor
import kotlin.math.pow

object Day04 : Day {
    override fun part1(input: List<String>): String {
        var sum = 0
        input.forEach { card ->
            val (winning, mine) = getCardNumbers(card)
            sum += floor(2.0.pow(mine.intersect(winning).size - 1)).toInt()
        }
        return sum.toString()
    }

    override fun part2(input: List<String>): String {
        val numScratchcards = IntArray(input.size) { 1 }
        input.forEachIndexed { cardIndex, card ->
            val (winning, mine) = getCardNumbers(card)
            (1..mine.intersect(winning).size).forEach { offset ->
                numScratchcards[cardIndex + offset] += numScratchcards[cardIndex]
            }
        }
        return numScratchcards.sum().toString()
    }

    private fun getCardNumbers(card: String): List<Set<Int>> =
        card.substringAfter(": ")
            .split(" | ")
            .map { str -> Regex("\\d+").findAll(str) }
            .map { seq -> seq.map { matchResult -> matchResult.value.toInt() }.toSet() }
}
