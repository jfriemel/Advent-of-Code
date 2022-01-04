package de.jfriemel.aoc;

import de.jfriemel.aoc.days.*;

import java.io.BufferedReader;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class Main {

    private static final Map<Integer, Day> DAYS = new HashMap<>();

    public static void main(String[] args) {
        final int day  = args.length >= 1 ? Integer.parseInt(args[0]) : 0;
        final int part = args.length >= 2 ? Integer.parseInt(args[1]) : 0;

        fillDaysMap();

        if (day >= 1 && day <= 25) {
            System.out.println(generateOutputText(day, part));
        } else {
            for (int d = 1; d <= 25; d++) {
                System.out.println(generateOutputText(d, part) + '\n');
            }
        }
    }

    private static String generateOutputText(final int day, final int part) {
        final List<String> input = loadInput(day);
        final StringBuilder outputBuilder = new StringBuilder();
        outputBuilder.append("Advent of Code 2021, Day ")
                     .append(day)
                     .append(", Part");
        if (part == 1) {
            outputBuilder.append(" 1\n")
                         .append(DAYS.get(day).part1(input));
        } else if (part == 2) {
            outputBuilder.append(" 2\n").append(DAYS.get(day).part2(input));
        } else {
            outputBuilder.append("s 1 and 2\n")
                         .append("Part 1: ")
                         .append(DAYS.get(day).part1(input))
                         .append("\nPart 2: ")
                         .append(DAYS.get(day).part2(input));
        }
        return outputBuilder.toString();
    }

    private static List<String> loadInput(final int day) {
        final String dayString = day < 10 ? '0' + Integer.toString(day) : Integer.toString(day);

        final List<String> input;
        final InputStream inputStream = Main.class.getResourceAsStream("/input" + dayString + ".txt");
        if (inputStream != null) {
            final BufferedReader bufferedReader = new BufferedReader(new InputStreamReader(inputStream));
            input = bufferedReader.lines().toList();
        } else {
            System.err.println("Input file 'input" + dayString + ".txt' not found. Proceeding with empty input.");
            input = new ArrayList<>();
        }

        return input;
    }

    private static void fillDaysMap() {
        DAYS.put(1,  new Day01());
        DAYS.put(2,  new Day02());
        DAYS.put(3,  new Day03());
        DAYS.put(4,  new Day04());
        DAYS.put(5,  new Day05());
        DAYS.put(6,  new Day06());
        DAYS.put(7,  new Day07());
        DAYS.put(8,  new Day08());
        DAYS.put(9,  new Day09());
        DAYS.put(10, new Day10());
        DAYS.put(11, new Day11());
        DAYS.put(12, new Day12());
        DAYS.put(13, new Day13());
        DAYS.put(14, new Day14());
        DAYS.put(15, new Day15());
        DAYS.put(16, new Day16());
        DAYS.put(17, new Day17());
        DAYS.put(18, new Day18());
        DAYS.put(19, new Day19());
        DAYS.put(20, new Day20());
        DAYS.put(21, new Day21());
        DAYS.put(22, new Day22());
        DAYS.put(23, new Day23());
        DAYS.put(24, new Day24());
        DAYS.put(25, new Day25());
    }

}