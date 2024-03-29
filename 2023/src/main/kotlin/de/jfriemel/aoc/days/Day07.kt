package de.jfriemel.aoc.days

import de.jfriemel.aoc.Day

object Day07 : Day {
    override fun part1(input: List<String>) = getTotalWinnings(input, false)

    override fun part2(input: List<String>) = getTotalWinnings(input, true)

    private fun getTotalWinnings(input: List<String>, part2: Boolean): String {
        val handsAndBids = input.map { line -> line.split(" ") }.map { (a, b) -> Pair(a, b.toInt()) }
        val comparator = Comparator<Pair<String, Int>> { handBid1, handBid2 ->
            val hand1 = handBid1.first
            val hand2 = handBid2.first
            if (getHandType(hand1, part2) != getHandType(hand2, part2)) {
                return@Comparator getHandType(hand1, part2) - getHandType(hand2, part2)
            }
            return@Comparator getHandValue(hand1, part2) - getHandValue(hand2, part2)
        }
        return handsAndBids.sortedWith(comparator).withIndex().sumOf {
                indexedValue -> (indexedValue.index + 1) * (indexedValue.value.second)
        }.toString()
    }

    private fun getHandType(hand: String, part2: Boolean): Int {
        if (part2 && 'J' in hand) {
            return "23456789TQKA".maxOf { replacement ->
                getHandType(hand.replaceFirst('J', replacement), true)
            }
        }
        val occurrences = mutableMapOf<Char, Int>()
        hand.forEach { card ->
            occurrences[card] = occurrences.getOrDefault(card, 0) + 1
        }
        if (occurrences.any { entry -> entry.value == 5 }) {
            return 6 // Five of a kind
        } else if (occurrences.any { entry -> entry.value == 4 }) {
            return 5 // Four of a kind
        } else if (occurrences.any { entry -> entry.value == 3 } && occurrences.any { entry -> entry.value == 2 }) {
            return 4 // Full house
        } else if (occurrences.any { entry -> entry.value == 3 }) {
            return 3 // Three of a kind
        } else if (occurrences.count { entry -> entry.value == 2 } == 2) {
            return 2 // Two pairs
        } else if (occurrences.any { entry -> entry.value == 2 }) {
            return 1 // One pair
        }
        return 0 // High card
    }

    private fun getHandValue(hand: String, part2: Boolean = false): Int {
        return hand.map { card ->
            when (card) {
                'A' -> 14
                'K' -> 13
                'Q' -> 12
                'J' -> if (part2) 1 else 11
                'T' -> 10
                else -> card.digitToInt()
            }
        }.fold(0) { acc, cardVal -> 15 * acc + cardVal }
    }
}
