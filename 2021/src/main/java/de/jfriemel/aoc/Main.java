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
        if (args.length < 2) {
            System.err.println("Two arguments required: [day] [part].");
        }

        int day = Integer.parseInt(args[0]);
        int part = Integer.parseInt(args[1]);

        List<String> input = loadInput(day);
        fillDaysMap();

        String result;
        String outputString = "Advent of Code 2021, Day " + day + ", Part";
        if (part == 1) {
            outputString = outputString + " 1\n";
            result = DAYS.get(day).part1(input);
        } else if (part == 2) {
            outputString = outputString + " 2\n";
            result = DAYS.get(day).part2(input);
        } else {
            outputString = outputString + "s 1 and 2\n";
            result = "Part 1: " + DAYS.get(day).part1(input) + "\nPart 2: " + DAYS.get(day).part2(input);
        }

        System.out.println(outputString);
        System.out.println(result);
    }

    private static List<String> loadInput(int day) {
        String dayString = String.valueOf(day);
        if (day < 10) {
            dayString = '0' + dayString;
        }

        List<String> input;
        InputStream inputStream = Main.class.getResourceAsStream("/input" + dayString + ".txt");
        if (inputStream != null) {
            BufferedReader bufferedReader = new BufferedReader(new InputStreamReader(inputStream));
            input = bufferedReader.lines().toList();
        } else {
            input = new ArrayList<>();
        }

        return input;
    }

    private static void fillDaysMap() {
        DAYS.put(1, new Day01());
        DAYS.put(2, new Day02());
        DAYS.put(3, new Day03());
    }

}
