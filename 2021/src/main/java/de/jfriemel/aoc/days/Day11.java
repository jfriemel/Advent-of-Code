package de.jfriemel.aoc.days;

import de.jfriemel.aoc.Day;

import java.util.Arrays;
import java.util.List;

public class Day11 implements Day {

    @Override
    public String part1(List<String> input) {
        int[][] grid = getGrid(input);
        int sum = 0;
        for (int i = 0; i < 100; i++) {
            sum += step(grid);
        }
        return Integer.toString(sum);
    }

    @Override
    public String part2(List<String> input) {
        int[][] grid = getGrid(input);
        int i = 0;
        do {
            i++;
        } while (step(grid) < 100);
        return Integer.toString(i);
    }

    private int step(int[][] grid) {
        int numFlashes = 0;
        boolean[][] flashed = new boolean[grid.length][grid[0].length];
        boolean changed;
        for (int i = 1; i < grid.length - 1; i++) {
            for (int j = 1; j < grid[i].length - 1; j++) {
                grid[i][j]++;
            }
        }
        do {
            changed = false;
            for (int i = 1; i < grid.length - 1; i++) {
                for (int j = 1; j < grid[i].length - 1; j++) {
                    if (grid[i][j] > 9) {
                        changed = true;
                        grid[i][j] = 0;
                        flashed[i][j] = true;
                        numFlashes++;
                        for (int row = -1; row <= 1; row++) {
                            for (int col = -1; col <= 1; col++) {
                                if (row == 0 && col == 0)   continue;
                                if (!flashed[i+row][j+col]) grid[i+row][j+col]++;
                            }
                        }
                    }
                }
            }
         } while (changed);
        return numFlashes;
    }

    private int[][] getGrid(List<String> input) {
        List<char[]> inputGrid = input.stream().map(String::toCharArray).toList();
        int[][] grid = new int[inputGrid.size() + 2][inputGrid.get(0).length + 2];
        for (int[] row : grid) {
            Arrays.fill(row, Integer.MIN_VALUE);
        }
        for (int i = 0; i < inputGrid.size(); i++) {
            for (int j = 0; j < inputGrid.get(i).length; j++) {
                grid[i+1][j+1] = Integer.parseInt(inputGrid.get(i)[j] + "");
            }
        }
        return grid;
    }

}