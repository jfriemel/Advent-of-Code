package de.jfriemel.aoc.days;

import de.jfriemel.aoc.Day;

import java.util.Arrays;
import java.util.List;

public class Day25 implements Day {

    @Override
    public String part1(List<String> input) {
        final int width = input.get(0).length();
        final int height = input.size();

        char[][] grid = new char[width][height];
        for (int x = 0; x < width; x++) {
            for (int y = 0; y < height; y++) {
                grid[x][y] = input.get(y).charAt(x);
            }
        }

        char[][] newGrid;
        boolean changed;
        int step = 0;
        do {
            newGrid = Arrays.stream(grid).map(char[]::clone).toArray(char[][]::new); // copy grid
            changed = false;
            step++;

            // Try to move all east-facing sea cucumbers east.
            for (int x = 0; x < width; x++) {
                for (int y = 0; y < height; y++) {
                    if (grid[x][y] == '>' && grid[(x + 1) % width][y] == '.') {
                        newGrid[x][y] = '.';
                        newGrid[(x + 1) % width][y] = '>';
                        changed = true;
                    }
                }
            }
            grid = newGrid;
            newGrid = Arrays.stream(grid).map(char[]::clone).toArray(char[][]::new); // copy grid

            // Try to move all south-facing sea cucumbers south.
            for (int x = 0; x < width; x++) {
                for (int y = 0; y < height; y++) {
                    if (grid[x][y] == 'v' && grid[x][(y + 1) % height] == '.') {
                        newGrid[x][y] = '.';
                        newGrid[x][(y + 1) % height] = 'v';
                        changed = true;
                    }
                }
            }
            grid = newGrid;
        } while (changed);

        return Integer.toString(step);
    }

    @Override
    public String part2(List<String> input) {
        return "Merry Christmas!";
    }

}