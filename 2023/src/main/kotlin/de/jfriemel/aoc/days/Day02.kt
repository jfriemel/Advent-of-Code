package de.jfriemel.aoc.days

import de.jfriemel.aoc.Day
import kotlin.math.max

object Day02 : Day {
    override fun part1(input: List<String>): String {
        val bagContains = mapOf("red" to 12, "green" to 13, "blue" to 14)
        return input.map { entry -> entry.substringAfter(": ") }.foldIndexed(0) { game, acc, draws ->
            draws.split("; ").forEach { draw ->
                draw.split(", ").map { it.split(" ") }.forEach { (num, color) ->
                    if (num.toInt() > bagContains[color]!!) {
                        return@foldIndexed acc
                    }
                }
            }
            acc + game + 1
        }.toString()
    }

    override fun part2(input: List<String>): String {
        return input.map { entry -> entry.split(": ")[1] }.fold(0) { acc, draws ->
            val powerMap = mutableMapOf("red" to 0, "green" to 0, "blue" to 0)
            draws.split("; ").forEach { draw ->
                draw.split(", ").map { it.split(" ") }.forEach { (num, color) ->
                    powerMap[color] = max(powerMap[color]!!, num.toInt())
                }
            }
            acc + powerMap["red"]!! * powerMap["green"]!! * powerMap["blue"]!!
        }.toString()
    }
}
