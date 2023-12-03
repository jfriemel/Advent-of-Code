package de.jfriemel.aoc.days

import de.jfriemel.aoc.Day

class Day01 : Day {
    override fun part1(input: List<String>): String {
        var sum = 0
        input.forEach { entry ->
            val digits = entry.filter { it.isDigit() }
            sum += "${digits.first()}${digits.last()}".toInt()
        }
        return sum.toString()
    }

    override fun part2(input: List<String>): String {
        var sum = 0
        input.forEach { entry ->
            val fullEntry = entry
                .replace("one", "one1one")
                .replace("two", "two2two")
                .replace("three", "three3three")
                .replace("four", "four4four")
                .replace("five", "five5five")
                .replace("six", "six6six")
                .replace("seven", "seven7seven")
                .replace("eight", "eight8eight")
                .replace("nine", "nine9nine")
            val digits = fullEntry.filter { it.isDigit() }
            sum += "${digits.first()}${digits.last()}".toInt()
        }
        return sum.toString()
    }
}
