package de.jfriemel.aoc.days

import de.jfriemel.aoc.Day
import kotlin.math.max

class Day02 : Day {
    override fun part1(input: List<String>): String {
        val bagContains = mapOf("red" to 12, "green" to 13, "blue" to 14)
        var sum = 0
        input.map { entry -> entry.split(": ")[1] }.forEachIndexed { game, draws ->
            var possible = true
            draws.split("; ").forEach { draw ->
                draw.split(", ").map { it.split(" ") }.forEach { colorDraw ->
                    if (colorDraw[0].toInt() > bagContains[colorDraw[1]]!!) {
                        possible = false
                    }
                }
            }
            if (possible) {
                sum += game + 1
            }
        }
        return sum.toString()
    }

    override fun part2(input: List<String>): String {
        var sum = 0
        input.map { entry -> entry.split(": ")[1] }.forEach { draws ->
            val powerMap = mutableMapOf("red" to 0, "green" to 0, "blue" to 0)
            draws.split("; ").forEach { draw ->
                draw.split(", ").map { it.split(" ") }.forEach { colorDraw ->
                    powerMap[colorDraw[1]] = max(powerMap[colorDraw[1]]!!, colorDraw[0].toInt())
                }
            }
            sum += powerMap["red"]!! * powerMap["green"]!! * powerMap["blue"]!!
        }
        return sum.toString()
    }
}
