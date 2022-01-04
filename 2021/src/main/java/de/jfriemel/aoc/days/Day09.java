package de.jfriemel.aoc.days;

import de.jfriemel.aoc.Day;

import java.util.*;

public class Day09 implements Day {

    @Override
    public String part1(List<String> input) {
        final int[][] grid = getGrid(input);
        final List<int[]> lowCoords = getLowCoords(grid);

        int sum = 0;
        for (final int[] coords : lowCoords) {
            sum += 1 + grid[coords[0]][coords[1]];
        }

        return Integer.toString(sum);
    }

    @Override
    public String part2(List<String> input) {
        final int[][] grid = getGrid(input);
        final List<int[]> lowCoords = getLowCoords(grid);
        final List<Set<List<Integer>>> basins = new ArrayList<>();

        for (final int[] coords : lowCoords) {
            // Generate the basins surrounding the low points.
            final Set<List<Integer>> spots = new HashSet<>();
            generateBasin(grid, spots, Arrays.asList(coords[0], coords[1]));
            basins.add(spots);
        }

        basins.sort(Comparator.comparingInt(s -> -s.size())); // Sort list from biggest to smallest basin.
        return Integer.toString(basins.get(0).size() * basins.get(1).size() * basins.get(2).size());
    }

    private int[][] getGrid(final List<String> input) {
        // Parses the input to a grid. The grid is padded on all four sides to avoid annoying edge cases at the edges of
        // the grid.
        final List<char[]> inputGrid = input.stream().map(String::toCharArray).toList();
        final int[][] grid = new int[inputGrid.size() + 2][inputGrid.get(0).length + 2];

        for (final int[] row : grid) {
            Arrays.fill(row, Integer.MAX_VALUE);
        }

        for (int i = 0; i < inputGrid.size(); i++) {
            for (int j = 0; j < inputGrid.get(i).length; j++) {
                grid[i+1][j+1] = Integer.parseInt(inputGrid.get(i)[j] + "");
            }
        }

        return grid;
    }

    private List<int[]> getLowCoords(final int[][] grid) {
        // Finds all low points in the grid.
        final List<int[]> lowCoords = new ArrayList<>();

        for (int i = 1; i < grid.length - 1; i++) {
            for (int j = 1; j < grid[i].length - 1; j++) {
                if (grid[i][j] < grid[i-1][j] && grid[i][j] < grid[i+1][j]
                 && grid[i][j] < grid[i][j-1] && grid[i][j] < grid[i][j+1]) {
                    lowCoords.add(new int[]{i, j});
                }
            }
        }

        return lowCoords;
    }

    private void generateBasin(final int[][] grid, final Set<List<Integer>> spots, final List<Integer> pos) {
        // Recursively adds neighbor spots to the basin.
        spots.add(pos);
        final int x = pos.get(0);
        final int y = pos.get(1);
        final int val = grid[x][y];

        if (grid[x-1][y] > val && grid[x-1][y] < 9)
            generateBasin(grid, spots, Arrays.asList(x-1, y));
        if (grid[x+1][y] > val && grid[x+1][y] < 9)
            generateBasin(grid, spots, Arrays.asList(x+1, y));
        if (grid[x][y-1] > val && grid[x][y-1] < 9)
            generateBasin(grid, spots, Arrays.asList(x, y-1));
        if (grid[x][y+1] > val && grid[x][y+1] < 9)
            generateBasin(grid, spots, Arrays.asList(x, y+1));
    }

}