package de.jfriemel.aoc.days

import de.jfriemel.aoc.Day

object Day15 : Day {
    override fun part1(input: List<String>) = input[0].split(",").sumOf { str -> hash(str) }.toString()

    override fun part2(input: List<String>): String {
        val boxes = Array<MutableList<Pair<String, Int>>>(256) { mutableListOf() }
        val labels = mutableSetOf<String>()
        input[0].split(",").forEach { entry ->
            val label = Regex("[a-z]+").find(entry)!!.value
            labels.add(label)
            val hash = hash(label)
            if ('-' in entry) {
                boxes[hash].removeIf { (boxLabel, _) -> boxLabel == label }
            } else {
                val focalLength = Regex("\\d").find(entry)!!.value.toInt()
                val indexInBox = boxes[hash].indexOfFirst { (boxLabel, _) -> boxLabel == label }
                if (indexInBox < 0) {
                    boxes[hash].addLast(Pair(label, focalLength))
                } else {
                    boxes[hash][indexInBox] = Pair(label, focalLength)
                }
            }
        }

        return boxes.mapIndexed { boxIndex, lenses ->
            lenses.mapIndexed { lensIndex, (_, focalLength) ->
                (boxIndex + 1) * (lensIndex + 1) * focalLength
            }.sum()
        }.sum().toString()
    }

    private fun hash(str: String) = str.fold(0) { acc, c -> (17 * (acc + c.code)) % 256 }
}
