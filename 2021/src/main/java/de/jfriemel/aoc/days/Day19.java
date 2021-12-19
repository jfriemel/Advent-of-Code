package de.jfriemel.aoc.days;

import de.jfriemel.aoc.Day;

import java.util.*;

class Position {
    // Data structure for the position of a beacon/scanner as well as the difference between positions.
    int x;
    int y;
    int z;

    Position(final int x, final int y, final int z) {
        this.x = x;
        this.y = y;
        this.z = z;
    }

    // equals and hashCode need to be overwritten to be able to use Position in a Set or a Map.
    @Override
    public boolean equals(Object obj) {
        if (obj == null || obj.getClass() != this.getClass())
            return false;
        final Position pos = (Position) obj;
        return this.x == pos.x && this.y == pos.y && this.z == pos.z;
    }

    @Override
    public int hashCode() {
        return Objects.hash(x, y, z);
    }
}

public class Day19 implements Day {

    @Override
    public String part1(List<String> input) {
        return Integer.toString(findBeacons(input, true));
    }

    @Override
    public String part2(List<String> input) {
        return Integer.toString(findBeacons(input, false));
    }

    private int findBeacons(final List<String> input, final boolean calcNumOfBeacons) {
        // Parse the input file to lists of lists of positions, where the inner list contains the positions of the
        // beacons from the viewpoint of one of the scanners.
        // Use a LinkedList instead of the default Arrays.stream list because we need mutability, i.e. we need to be
        // able to remove elements.
        final List<List<Position>> scans = new LinkedList<>(
                Arrays.stream(String.join("\n", input).split("\n\n"))
                      .map(s -> Arrays.stream(s.substring(s.indexOf('\n') + 1).split("\n"))
                                      .map(this::parsePosition).toList())
                      .toList());

        final Set<Position> beacons = new HashSet<>(scans.get(0));
        scans.remove(0);

        // Idea: Store all the beacon's positions relative to scanner 0 in the set beacons. Whenever we find a scanner
        //       rotation that matches 12 of the previously seen beacons, we add all of that scanner's beacon positions
        //       to the set and remove the scanner's scan from the scans list.
        //       Once the scans list is empty, all beacons will have been added to the beacons set.

        // We collect all of the scanner's positions (relative to scanner 0) in a list to be able to compute the maximum
        // distance between any two scanners for part 2.
        final List<Position> scannerDistances = new ArrayList<>();

        int index = 0; // Index of the scanner that is currently analyzed.
        scanLoop:
        while (!scans.isEmpty()) {
            final List<Position> scan = scans.get(index);
            final List<List<Position>> rotations = generateRotations(scan);

            for (List<Position> rotation : rotations) {
                // Calculate the position differences between any beacon position from the current scanner rotation and
                // any beacon from the set of beacon positions relative to scanner 0.
                // If any difference occurs 12 times, we have a match, and we can accurately compute the positions of
                // all beacons in the current scanner rotation relative to scanner 0.
                final Map<Position, Integer> numOfDistances = new HashMap<>();
                for (final Position beacon : beacons) {
                    for (final Position otherBeacon : rotation) {
                        final Position difference = new Position(otherBeacon.x - beacon.x,
                                                                 otherBeacon.y - beacon.y,
                                                                 otherBeacon.z - beacon.z);
                        numOfDistances.put(difference, numOfDistances.getOrDefault(difference, 0) + 1);
                    }
                }

                final Set<Position> allDifferences = numOfDistances.keySet();
                for (final Position diff : allDifferences) {
                    if (numOfDistances.get(diff) >= 12) {
                        rotation = rotation.stream().peek(p -> { // Make the beacon positions relative to scanner 0.
                            p.x -= diff.x;
                            p.y -= diff.y;
                            p.z -= diff.z;
                        }).toList();
                        beacons.addAll(rotation);   // Add the current scanner's beacons to the set of beacons.
                        scans.remove(index);        // Remove the current scanner from the list of unprocessed scanners.
                        scannerDistances.add(diff);
                        index = 0;
                        continue scanLoop;          // Continue with the next scanner until no scanner is left.
                    }
                }
            }
            index++;
        }

        if (calcNumOfBeacons) {
            return beacons.size(); // Part 1.
        }

        // Find the largest distance between any two scanners.
        int maxDist = 0;
        for (int i = 0; i < scannerDistances.size(); i++) {
            for (int j = i + 1; j < scannerDistances.size(); j++) {
                final Position pos1 = scannerDistances.get(i);
                final Position pos2 = scannerDistances.get(j);
                final int dist = Math.abs(pos1.x - pos2.x) + Math.abs(pos1.y - pos2.y) + Math.abs(pos1.z - pos2.z);
                if (dist > maxDist)
                    maxDist = dist;
            }
        }
        return maxDist; // Part 2.
    }

    List<List<Position>> generateRotations(final List<Position> coordinates) {
        List<List<Position>> rotations = new ArrayList<>(24);
        for (int i = 0; i < 24; i++) {
            rotations.add(new ArrayList<>());
        }

        for (final Position pos : coordinates) {
            // Yes, this is very clumsy. There is probably a beautiful three-line solution with rotation matrices,
            // but this works just as well.
            rotations.get( 0).add(new Position( pos.x,  pos.y,  pos.z)); // Facing towards +x, no rotation.
            rotations.get( 1).add(new Position( pos.x,  pos.z, -pos.y)); // Facing towards +x, 90-degree rotation.
            rotations.get( 2).add(new Position( pos.x, -pos.y, -pos.z)); // Facing towards +x, 180-degree rotation.
            rotations.get( 3).add(new Position( pos.x, -pos.z,  pos.y)); // Facing towards +x, 270-degree rotation.
            rotations.get( 4).add(new Position(-pos.x, -pos.y,  pos.z)); // Facing towards -x, no rotation.
            rotations.get( 5).add(new Position(-pos.x,  pos.z,  pos.y)); // Facing towards -x, 90-degree rotation.
            rotations.get( 6).add(new Position(-pos.x,  pos.y, -pos.z)); // Facing towards -x, 180-degree rotation.
            rotations.get( 7).add(new Position(-pos.x, -pos.z, -pos.y)); // Facing towards -x, 270-degree rotation.
            rotations.get( 8).add(new Position( pos.y, -pos.x,  pos.z)); // Facing towards +y, no rotation.
            rotations.get( 9).add(new Position( pos.y,  pos.z,  pos.x)); // Facing towards +y, 90-degree rotation.
            rotations.get(10).add(new Position( pos.y,  pos.x, -pos.z)); // Facing towards +y, 180-degree rotation.
            rotations.get(11).add(new Position( pos.y, -pos.z, -pos.x)); // Facing towards +y, 270-degree rotation.
            rotations.get(12).add(new Position(-pos.y,  pos.x,  pos.z)); // Facing towards -y, no rotation.
            rotations.get(13).add(new Position(-pos.y,  pos.z, -pos.x)); // Facing towards -y, 90-degree rotation.
            rotations.get(14).add(new Position(-pos.y, -pos.x, -pos.z)); // Facing towards -y, 180-degree rotation.
            rotations.get(15).add(new Position(-pos.y, -pos.z,  pos.x)); // Facing towards -y, 270-degree rotation.
            rotations.get(16).add(new Position( pos.z,  pos.y, -pos.x)); // Facing towards +z, no rotation.
            rotations.get(17).add(new Position( pos.z, -pos.x, -pos.y)); // Facing towards +z, 90-degree rotation.
            rotations.get(18).add(new Position( pos.z, -pos.y,  pos.x)); // Facing towards +z, 180-degree rotation.
            rotations.get(19).add(new Position( pos.z,  pos.x,  pos.y)); // Facing towards +z, 270-degree rotation.
            rotations.get(20).add(new Position(-pos.z, -pos.y, -pos.x)); // Facing towards -z, no rotation.
            rotations.get(21).add(new Position(-pos.z, -pos.x,  pos.y)); // Facing towards -z, 90-degree rotation.
            rotations.get(22).add(new Position(-pos.z,  pos.y,  pos.x)); // Facing towards -z, 180-degree rotation.
            rotations.get(23).add(new Position(-pos.z,  pos.x, -pos.y)); // Facing towards -z, 270-degree rotation.
            // My right hand hurts from visualizing all those rotations.
        }

        return rotations;
    }

    Position parsePosition(final String line) {
        final String[] parts = line.split(",");
        return new Position(Integer.parseInt(parts[0]), Integer.parseInt(parts[1]), Integer.parseInt(parts[2]));
    }

}