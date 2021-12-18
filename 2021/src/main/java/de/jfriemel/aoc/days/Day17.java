package de.jfriemel.aoc.days;

import de.jfriemel.aoc.Day;

import java.util.List;

public class Day17 implements Day {

    @Override
    public String part1(List<String> input) {
        return Integer.toString(launchProbe(input, false));
    }

    @Override
    public String part2(List<String> input) {
        return Integer.toString(launchProbe(input, true));
    }

    private int launchProbe(final List<String> input, final boolean all) {
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

        // The target is in positive x direction, so the initial x velocity must be positive.
        for (int initVelX = 1; initVelX <= maxX; initVelX++) {
            // The target can never be reached if the initial y velocity instantly launches the probe below the target.
            for (int initVelY = minY; initVelY < 1000; initVelY++) {
                int x = 0;
                int y = 0;
                int velX = initVelX;
                int velY = initVelY;
                int bestYRound = 0;

                while (y >= minY) { // When y is lower than minY, the target cannot be reached anymore.

                    x += velX;
                    y += velY;
                    if (y > bestYRound) // Determine the highest y in the current probe launch.
                        bestYRound = y;

                    if (x >= minX && x <= maxX && y >= minY && y <= maxY) { // x and y are within the target area.
                        numberOfSolutions++;
                        if (bestYRound > bestY)
                            bestY = bestYRound;
                        break;
                    }

                    if (velX > 0)
                        velX--; // Since initVelX > 0, we don't need to check for velX < 0.
                    velY--;
                }
            }
        }

        if (all)
            return numberOfSolutions;
        return bestY;
    }

}