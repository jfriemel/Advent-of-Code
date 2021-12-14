package de.jfriemel.aoc.days;

import de.jfriemel.aoc.Day;

import java.util.List;

public class Day13 implements Day {

    @Override
    public String part1(List<String> input) {
        return fold(input, true);
    }

    @Override
    public String part2(List<String> input) {
        return fold(input, false);
    }

    private String fold(final List<String> input, boolean once) {
        final String inputStr = String.join("\n", input);
        final String[] parts = inputStr.split("\n\n");
        final String[] coordsArr = parts[0].split("\n");

        boolean[][] grid = new boolean[2000][2000];

        for (final String coords : coordsArr) {
            final String[] split = coords.split(",");
            grid[Integer.parseInt(split[0])][Integer.parseInt(split[1])] = true;
        }

        String[] commands = parts[1].split("\n");
        if (once)
            commands = new String[]{commands[0]};
        int xBound = 2000;
        int yBound = 2000;

        for (String command : commands) {
            command = command.substring(11);

            if (command.charAt(0) == 'x') {
                final int prevBound = xBound;
                xBound = Integer.parseInt(command.split("=")[1]);
                for (int i = 1; xBound + i < prevBound && xBound - i >= 0; i++) {
                    for (int y = 0; y < yBound; y++) {
                        grid[xBound-i][y] |= grid[xBound+i][y];
                    }
                }
            } else {
                final int prevBound = yBound;
                yBound = Integer.parseInt(command.split("=")[1]);
                for (int i = 1; yBound + i < prevBound && yBound - i >= 0; i++) {
                    for (int x = 0; x < xBound; x++) {
                        grid[x][yBound-i] |= grid[x][yBound+i];
                    }
                }
            }
        }

        int sum = 0;
        final StringBuilder output = new StringBuilder();
        for (int y = 0; y < yBound; y++) {
            output.append('\n');
            for (int x = 0; x < xBound; x++) {
                if (grid[x][y] && once)
                    sum++;
                else
                    output.append(grid[x][y] ? '\u2588' : ' ');
            }
        }

        if (once)
            return Integer.toString(sum);
        return output.toString();
    }

}