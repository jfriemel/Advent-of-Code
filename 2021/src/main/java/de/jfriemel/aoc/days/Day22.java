package de.jfriemel.aoc.days;

import de.jfriemel.aoc.Day;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

class Cuboid {
    // Data structure for a cuboid containing cubes of the reactor core.

    // 3d intervals of the cuboid:
    long minX;
    long maxX;
    long minY;
    long maxY;
    long minZ;
    long maxZ;

    Cuboid(final long minX, final long maxX, final long minY, final long maxY, final long minZ, final long maxZ) {
        this.minX = minX;
        this.maxX = maxX;
        this.minY = minY;
        this.maxY = maxY;
        this.minZ = minZ;
        this.maxZ = maxZ;
    }

    long getSize() {
        // Calculate the size of the cuboid, i.e. the number of cubes contained in the cuboid.
        return (maxX - minX + 1) * (maxY - minY + 1) * (maxZ - minZ + 1);
    }

    boolean intersects(final Cuboid other) {
        // Check whether this cuboid intersects with the given other cuboid.
        final boolean intersectsX = this.minX <= other.maxX && this.maxX >= other.minX;
        final boolean intersectsY = this.minY <= other.maxY && this.maxY >= other.minY;
        final boolean intersectsZ = this.minZ <= other.maxZ && this.maxZ >= other.minZ;
        return intersectsX && intersectsY && intersectsZ;
    }

    Cuboid getIntersection(final Cuboid other) {
        // Return a new cuboid that is the intersection of this cuboid and the given other cuboid.
        final long minX = Math.max(this.minX, other.minX);
        final long minY = Math.max(this.minY, other.minY);
        final long minZ = Math.max(this.minZ, other.minZ);
        final long maxX = Math.min(this.maxX, other.maxX);
        final long maxY = Math.min(this.maxY, other.maxY);
        final long maxZ = Math.min(this.maxZ, other.maxZ);
        return new Cuboid(minX, maxX, minY, maxY, minZ, maxZ);
    }
}

public class Day22 implements Day {

    @Override
    public String part1(List<String> input) {
        return Long.toString(countActiveCubes(input, true));
    }

    @Override
    public String part2(List<String> input) {
        return Long.toString(countActiveCubes(input, false));
    }

    private long countActiveCubes(final List<String> input, final boolean initRegionOnly) {
        // Idea: Use the principle of inclusion-exclusion. In other words, keep track of all cuboids that are 'on'. When
        //       two 'on' cuboids overlap, add keep track of their intersection and subtract the intersection's value
        //       from the final result.
        final List<Cuboid> posCuboids = new ArrayList<>();
        final List<Cuboid> negCuboids = new ArrayList<>();

        for (final String line : input) {
            final String[] command = line.split(" ");
            final boolean on = command[0].equals("on");
            List<Long> parts = Arrays.stream(command[1].substring(2).split("(,[xyz]=)|\\.\\."))
                                     .map(Long::parseLong)
                                     .toList();

            if (initRegionOnly) {
                // For part 1, only consider the cuboids in the -50 to 50 initialization region.
                parts = parts.stream().filter(l -> Math.abs(l) <= 50).toList();
                if (parts.size() < 6) {
                    continue;
                }
            }

            final Cuboid cuboid = new Cuboid(parts.get(0), parts.get(1),  // minX, maxX
                                             parts.get(2), parts.get(3),  // minY, maxY
                                             parts.get(4), parts.get(5)); // minZ, maxZ


            // Find all intersections of the current cuboid with previously seen cuboids.
            // Inclusion-exclusion: Add the intersections with the + cuboids to the - list and
            //                      add the intersections with the - cuboids to the + list.
            final List<Cuboid> posIntersects = findIntersections(cuboid, posCuboids);
            final List<Cuboid> negIntersects = findIntersections(cuboid, negCuboids);

            posCuboids.addAll(negIntersects);
            negCuboids.addAll(posIntersects);
            if (on) {
                posCuboids.add(cuboid);
            }
        }

        return posCuboids.stream().map(Cuboid::getSize).reduce(Long::sum).orElseThrow()
             - negCuboids.stream().map(Cuboid::getSize).reduce(Long::sum).orElseThrow();
    }

    private List<Cuboid> findIntersections(final Cuboid cuboid, final List<Cuboid> cuboids) {
        return cuboids.stream().filter(cuboid::intersects).map(cuboid::getIntersection).toList();
    }

}