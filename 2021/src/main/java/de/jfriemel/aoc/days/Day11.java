package de.jfriemel.aoc.days;

import de.jfriemel.aoc.Day;

import java.util.Arrays;
import java.util.List;

public class Day11 implements Day {

    @Override
    public String part1(List<String> input) {
        final int[][] grid = getGrid(input);

        int sum = 0;
        for (int i = 0; i < 100; i++) {
            sum += step(grid);
        }

        return Integer.toString(sum);
    }

    @Override
    public String part2(List<String> input) {
        final int[][] grid = getGrid(input);

        int i = 0;
        do i++; while (step(grid) < 100); // If step(grid) == 100, all octopuses have flashed on a 10x10 grid.

        return Integer.toString(i);
    }

    private int step(final int[][] grid) {
        // Does a single step and returns the number of flashes that have occurred during this step.

        for (int i = 1; i < grid.length - 1; i++) {
            for (int j = 1; j < grid[i].length - 1; j++) {
                grid[i][j]++;
            }
        }

        int numFlashes = 0;
        boolean changed;
        do {
            // Iterate over the grid until all flashes have occurred.
            changed = false;

            for (int i = 1; i < grid.length - 1; i++) {
                for (int j = 1; j < grid[i].length - 1; j++) {
                    if (grid[i][j] > 9) {
                        changed = true;
                        grid[i][j] = 0;
                        numFlashes++;

                        for (int row = -1; row <= 1; row++) {
                            for (int col = -1; col <= 1; col++) {
                                if (row == 0 && col == 0) // Not a neighbor, skip.
                                    continue;
                                if (grid[i+row][j+col] > 0) // Neighbors == 0 have already flashed, ignore them.
                                    grid[i+row][j+col]++;
                            }
                        }
                    }
                }
            }
         } while (changed);

        return numFlashes;
    }

    private int[][] getGrid(final List<String> input) {
        // Parses the input to a grid. The grid is padded on all four sides to avoid annoying edge cases at the edges of
        // the grid.
        final List<char[]> inputGrid = input.stream().map(String::toCharArray).toList();
        final int[][] grid = new int[inputGrid.size() + 2][inputGrid.get(0).length + 2];

        for (final int[] row : grid) {
            Arrays.fill(row, 0);
        }

        for (int i = 0; i < inputGrid.size(); i++) {
            for (int j = 0; j < inputGrid.get(i).length; j++) {
                grid[i+1][j+1] = Integer.parseInt(inputGrid.get(i)[j] + "");
            }
        }

        return grid;
    }

}