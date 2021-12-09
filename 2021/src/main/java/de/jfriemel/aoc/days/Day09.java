package de.jfriemel.aoc.days;

import de.jfriemel.aoc.Day;

import java.util.*;

public class Day09 implements Day {

    @Override
    public String part1(List<String> input) {
        int[][] grid = getGrid(input);
        List<int[]> lowCoords = getLowCoords(grid);
        int sum = 0;
        for (int[] coords : lowCoords) {
            sum += 1 + grid[coords[0]][coords[1]];
        }
        return Integer.toString(sum);
    }

    @Override
    public String part2(List<String> input) {
        int[][] grid = getGrid(input);
        List<int[]> lowCoords = getLowCoords(grid);
        List<Set<List<Integer>>> basins = new ArrayList<>();
        for (int[] coords : lowCoords) {
            Set<List<Integer>> spots = new HashSet<>();
            generateBasin(grid, spots, Arrays.asList(coords[0], coords[1]));
            basins.add(spots);
        }
        basins.sort(Comparator.comparingInt(Set::size));
        int size = basins.size();
        return Integer.toString(basins.get(size-1).size() * basins.get(size-2).size() * basins.get(size-3).size());
    }

    private int[][] getGrid(List<String> input) {
        List<char[]> inputGrid = input.stream().map(String::toCharArray).toList();
        int[][] grid = new int[inputGrid.size() + 2][inputGrid.get(0).length + 2];
        for (int[] row : grid) {
            Arrays.fill(row, Integer.MAX_VALUE);
        }
        for (int i = 0; i < inputGrid.size(); i++) {
            for (int j = 0; j < inputGrid.get(i).length; j++) {
                grid[i+1][j+1] = Integer.parseInt(inputGrid.get(i)[j] + "");
            }
        }
        return grid;
    }

    private List<int[]> getLowCoords(int[][] grid) {
        List<int[]> lowCoords = new ArrayList<>();
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

    private void generateBasin(int[][] grid, Set<List<Integer>> spots, List<Integer> pos) {
        spots.add(pos);
        int x = pos.get(0);
        int y = pos.get(1);
        int val = grid[x][y];
        if (grid[x-1][y] > val && grid[x-1][y] < 9) {
            generateBasin(grid, spots, Arrays.asList(x-1, y));
        }
        if (grid[x+1][y] > val && grid[x+1][y] < 9) {
            generateBasin(grid, spots, Arrays.asList(x+1, y));
        }
        if (grid[x][y-1] > val && grid[x][y-1] < 9) {
            generateBasin(grid, spots, Arrays.asList(x, y-1));
        }
        if (grid[x][y+1] > val && grid[x][y+1] < 9) {
            generateBasin(grid, spots, Arrays.asList(x, y+1));
        }
    }

}