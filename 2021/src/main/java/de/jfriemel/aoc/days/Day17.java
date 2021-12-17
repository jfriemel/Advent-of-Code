package de.jfriemel.aoc.days;

import de.jfriemel.aoc.Day;

import java.util.List;

public class Day17 implements Day {

    @Override
    public String part1(List<String> input) {
        return Integer.toString(fireProbe(input, false));
    }

    @Override
    public String part2(List<String> input) {
        return Integer.toString(fireProbe(input, true));
    }

    private int fireProbe(final List<String> input, final boolean all) {
        // Determine the target coordinates from the puzzle input.
        final String[] parsed = input.get(0).substring(15).split(", y=");
        final String[][] parts = {parsed[0].split("\\.\\."), parsed[1].split("\\.\\.")};
        final int minX = Integer.parseInt(parts[0][0]);
        final int maxX = Integer.parseInt(parts[0][1]);
        final int minY = Integer.parseInt(parts[1][0]);
        final int maxY = Integer.parseInt(parts[1][1]);

        int bestY = 0;             // Highest y position seen in any successful launch.
        int numberOfSolutions = 0; // Number of initial velocities that bring the probe to the target.

        // Simple brute force over all sensible initial velocities for x and y.
        for (int initVelX = 1; initVelX <= maxX; initVelX++) {
            for (int initVelY = -1000; initVelY < 1000; initVelY++) {
                int x = 0;
                int y = 0;
                int velX = initVelX;
                int velY = initVelY;
                int bestYRound = 0;

                while (y >= minY) {
                    // When y is lower than minY, the target cannot be hit anymore.

                    x += velX;
                    y += velY;
                    if (y > bestYRound) // Determine the highest y in the current probe launch.
                        bestYRound = y;

                    if (x >= minX && x <= maxX && y >= minY && y <= maxY) {
                        // x and y are within the target.
                        numberOfSolutions++;
                        if (bestYRound > bestY)
                            bestY = bestYRound;
                        break;
                    }

                    if (velX > 0)
                        velX--;
                    else if (velX < 0)
                        velX++;
                    velY--;
                }
            }
        }

        if (all)
            return numberOfSolutions;
        return bestY;
    }

}