package de.jfriemel.aoc.days

import de.jfriemel.aoc.Day
import kotlin.math.max

object Day02 : Day {
    override fun part1(input: List<String>): String {
        val bagContains = mapOf("red" to 12, "green" to 13, "blue" to 14)
        var sum = 0
        input.map { entry -> entry.substringAfter(": ") }.forEachIndexed { game, draws ->
            draws.split("; ").forEach { draw ->
                draw.split(", ").map { it.split(" ") }.forEach { (num, color) ->
                    if (num.toInt() > bagContains[color]!!) {
                        return@forEachIndexed
                    }
                }
            }
            sum += game + 1
        }
        return sum.toString()
    }

    override fun part2(input: List<String>): String {
        var sum = 0
        input.map { entry -> entry.split(": ")[1] }.forEach { draws ->
            val powerMap = mutableMapOf("red" to 0, "green" to 0, "blue" to 0)
            draws.split("; ").forEach { draw ->
                draw.split(", ").map { it.split(" ") }.forEach { (num, color) ->
                    powerMap[color] = max(powerMap[color]!!, num.toInt())
                }
            }
            sum += powerMap["red"]!! * powerMap["green"]!! * powerMap["blue"]!!
        }
        return sum.toString()
    }
}
