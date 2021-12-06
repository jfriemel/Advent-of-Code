package de.jfriemel.aoc.days;

import de.jfriemel.aoc.Day;

import java.util.List;

public class Day02 implements Day {

    @Override
    public String part1(final List<String> input) {
        int x = 0;
        int y = 0;
        for (final String line : input) {
            String[] command = line.split(" ");
            int value = Integer.parseInt(command[1]);
            switch (command[0]) {
                case "forward" -> x += value;
                case "down" -> y += value;
                case "up" -> y -= value;
            }
        }
        return Integer.toString(x * y);
    }

    @Override
    public String part2(final List<String> input) {
        int x = 0;
        int y = 0;
        int aim = 0;
        for (final String line : input) {
            String[] command = line.split(" ");
            int value = Integer.parseInt(command[1]);
            switch (command[0]) {
                case "forward" -> {
                    x += value;
                    y += aim * value;
                }
                case "down" -> aim += value;
                case "up" -> aim -= value;
            }
        }
        return Integer.toString(x * y);
    }

}