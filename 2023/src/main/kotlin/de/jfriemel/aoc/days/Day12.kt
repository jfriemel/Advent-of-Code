package de.jfriemel.aoc.days

import de.jfriemel.aoc.Day

object Day12 : Day {
    override fun part1(input: List<String>): String {
        return input.map { line -> line.split(" ") }.sumOf { (springs, blocksRaw) ->
            val blocks = blocksRaw.split(",").map { block -> block.toInt() }
            countArrangements(springs, blocks, 0, 0, 0, mutableMapOf())
        }.toString()
    }

    override fun part2(input: List<String>): String {
        return input.map { line -> line.split(" ") }.sumOf { (springsRaw, blocksRaw) ->
            val springs = springsRaw.plus("?").repeat(5).dropLast(1)
            val blocks = blocksRaw.plus(",")
                .repeat(5)
                .split(",")
                .dropLast(1)
                .map { block -> block.toInt() }
            countArrangements(springs, blocks, 0, 0, 0, mutableMapOf())
        }.toString()
    }

    private fun countArrangements(
        springs: String,
        blocks: List<Int>,
        sIndex: Int,
        bIndex: Int,
        currentBlockSize: Int,
        cache: MutableMap<Triple<Int, Int, Int>, Long>,
    ): Long {
        val key = Triple(sIndex, bIndex, currentBlockSize)
        cache[key]?.let { return it }

        if (sIndex == springs.length) {
            return if ((bIndex == blocks.size - 1 && currentBlockSize == blocks[bIndex])
                || (bIndex == blocks.size && currentBlockSize == 0)) 1L else 0L
        }

        val result = ".#".sumOf { c ->
            if (springs[sIndex] != c && springs[sIndex] != '?') {
                0L
            } else if (c == '.' && currentBlockSize == 0) {
                countArrangements(springs, blocks, sIndex + 1, bIndex, 0, cache)
            } else if (c == '.' && bIndex < blocks.size && blocks[bIndex] == currentBlockSize) {
                countArrangements(springs, blocks, sIndex + 1, bIndex + 1, 0, cache)
            } else if (c == '#') {
                countArrangements(springs, blocks, sIndex + 1, bIndex, currentBlockSize + 1, cache)
            } else {
                0L
            }
        }

        cache[key] = result
        return result
    }
}
