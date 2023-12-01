package de.jfriemel.aoc

import de.jfriemel.aoc.days.Day01
import java.io.File

val dayArray: Array<Day> = arrayOf(Day01())

fun main(args: Array<String>) {
    println("Advent of Code 2023")
    if (args.isNotEmpty()) {
        runDay(args[0].toInt())
    } else {
        (1..1).forEach { day -> runDay(day) }
    }
}

private fun runDay(day: Int) {
    println("=".repeat(32))
    println("Day $day")
    val input = File(
        ClassLoader
            .getSystemClassLoader()
            .getResource("input${day.toString().padStart(2, '0')}.txt")!!
            .file,
    ).readLines()
    println("Part 1: ${dayArray[day - 1].part1(input)}")
    println("Part 2: ${dayArray[day - 1].part2(input)}")
    println("=".repeat(32))
}
