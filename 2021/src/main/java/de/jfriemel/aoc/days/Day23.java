package de.jfriemel.aoc.days;

import de.jfriemel.aoc.Day;

import java.util.*;

public class Day23 implements Day {

    // This solution takes some time (up to 8s on my machine) and eats RAM like Chrome (up to 4-5GB). Oh well...

    @Override
    public String part1(List<String> input) {
        return runInput(input);
    }

    @Override
    public String part2(List<String> input) {
        return runInput(new ArrayList<>(Arrays.asList(input.get(2), "  #D#C#B#A#", "  #D#B#A#C#", input.get(3))));
    }

    private String runInput(final List<String> input) {
        // This is basically Dijkstra. The PriorityQueue ensures that the first solution that is found is also the
        // smallest possible solution.
        PriorityQueue<RoomState> queue = new PriorityQueue<>(8192, Comparator.comparing(RoomState::getTotalCost));
        queue.add(parseInput(input));
        return Integer.toString(traverseStates(queue));
    }

    private RoomState parseInput(final List<String> input) {
        // Only the alphabetic chars from the input list are relevant as they describe where the amphipods are located.
        final List<Character> amphipods = String.join("", input)
                                                .chars()
                                                .mapToObj(i -> (char) i)
                                                .filter(Character::isAlphabetic)
                                                .toList();

        final char[] correct = {'A', 'B', 'C', 'D'};
        final char[] hallway = new char[11];
        final char[][] rooms = new char[4][4];

        Arrays.fill(hallway, '.');

        if (amphipods.size() == 8) {
            // For part 1, fill up the bottom two rows of rooms with the correct amphipods.
            for (int i = 0; i < 4; i++) {
                for (int j = 0; j < 2; j++) {
                    rooms[i][j] = amphipods.get(j * 4 + i);
                    rooms[i][j+2] = correct[i];
                }
            }
        } else {
            for (int i = 0; i < 4; i++) {
                for (int j = 0; j < 4; j++) {
                    rooms[i][j] = amphipods.get(j * 4 + i);
                }
            }
        }

        return new RoomState(hallway, rooms, 0);
    }

    private int traverseStates(final PriorityQueue<RoomState> queue) {
        // Idea: Take the RoomState with the lowest current cost and add all RoomStates that can be reached from that
        //       state to the PriorityQueue. Once a RoomState is done, return the total cost of that solution.
        while (!queue.isEmpty()) {
            final RoomState current = queue.poll();
            if (current.isDone()) {
                return current.getTotalCost();
            }
            queue.addAll(current.nextStates());
        }
        return -1; // This should never be reached.
    }

}

class RoomState {
    /* Corridor and room indices (terrible visualization, I know...):
     * #####################
     * #01 2 3 4 5 6 7 8 9X#
     * ###0,0#1,0#2,0#3,0###
     *   #0,1#1,1#2,1#3,1#
     *   #0,2#1,2#2,2#3,2#
     *   #0,3#1,3#2,3#3,3#
     *   #################   */
    char[] hallway = new char[11];
    char[][] rooms = new char[4][4];

    // Keep track of the total cost to reach the current configuration from the initial RoomState.
    int totalCost;

    final char[] correct = {'A', 'B', 'C', 'D'};
    final int[] costs = {1, 10, 100, 1000};

    int getTotalCost() {
        return totalCost;
    }

    RoomState(final char[] hallway, final char[][] rooms, final int totalCost) {
        this.hallway = hallway;
        this.rooms = rooms;
        this.totalCost = totalCost;
        tryMoveToDestinations();
    }

    void tryMoveToDestinations() {
        // Moves all amphipods in the hallway whose destination rooms are reachable to their destination rooms.

        hallwayLoop: // Check all non-empty spaces in the hallway, add all possible moves.
        for (int i = 0; i < 11; i++) {
            if (hallway[i] != '.') {
                int dest = hallway[i] - 'A';

                // Check if path to destination room is clear.
                if (i < 2 * dest + 2) {
                    for (int j = i + 1; j <= 2 * dest + 2; j++) {
                        if (hallway[j] != '.')
                            continue hallwayLoop;
                    }
                } else {
                    for (int j = i - 1; j >= 2 * dest + 2; j--) {
                        if (hallway[j] != '.')
                            continue hallwayLoop;
                    }
                }

                int j = 3;
                while (j >= 0 && rooms[dest][j] != '.') {
                    // If any space in the room is occupied by a 'wrong' amphipod, it is not yet a suitable destination.
                    if (rooms[dest][j] != correct[dest])
                        continue hallwayLoop;
                    j--;
                }
                if (j >= 0) {
                    rooms[dest][j] = hallway[i];
                    hallway[i] = '.';
                    totalCost += (Math.abs(2 * dest + 2 - i) + j + 1) * costs[dest];
                    i = -1; // Since an amphipod has moved to a room, check all hallway spaces again.
                }
            }
        }
    }

    List<RoomState> nextStates() {
        // Finds all possible RoomStates that can be reached from the current RoomState.
        // Since tryMoveToDestinations() has been called before, the only RoomStates that can be reached in a single
        // step are those where an amphipod moves from a room to the hallway.
        final List<RoomState> nextStates = new ArrayList<>();

        for (int i = 0; i < 4; i++) {
            boolean needsMoves = false;
            // needsMoves is only true if at least one amphipod in the current room is in the wrong room.
            // Otherwise, all amphipods in the room are correctly placed and no further move is necessary.
            int topMost = 3;
            for (int j = 3; j >= 0; j--) {
                if (rooms[i][j] != '.') {
                    topMost = j;
                    if (rooms[i][j] != correct[i])
                        needsMoves = true;
                }
            }

            if (!needsMoves)
                continue;

            final int hallwayPos = 2 * i + 2;
            // See the visualization above to convince yourself that this is the correct hallway position corresponding
            // to the current room position.

            final List<Integer> possibleSpaces = new ArrayList<>();

            // Determine all hallway positions that can be reached from the current room.
            for (int j = hallwayPos; j < 11; j++) {
                if (hallway[j] != '.')
                    break;
                if (j % 2 != 0 || j % 10 == 0)
                    possibleSpaces.add(j);
            }
            for (int j = hallwayPos; j >= 0; j--) {
                if (hallway[j] != '.')
                    break;
                if (j % 2 != 0 || j % 10 == 0)
                    possibleSpaces.add(j);
            }

            for (final int j : possibleSpaces) {
                // Create RoomStates for all hallway positions that can be reached from the current room.
                final char[] newHallway = Arrays.copyOf(hallway, hallway.length);
                newHallway[j] = rooms[i][topMost];
                final char[][] newRooms = Arrays.stream(rooms).map(char[]::clone).toArray(char[][]::new);
                newRooms[i][topMost] = '.';
                final int newCost = totalCost + (Math.abs(hallwayPos - j) + topMost + 1) * costs[rooms[i][topMost] - 'A'];
                nextStates.add(new RoomState(newHallway, newRooms, newCost));
            }
        }

        return nextStates;
    }

    boolean isDone() {
        // Checks if all amphipods are in their correct rooms and no amphipod is on the hallway.
        for (final char space : hallway) {
            if (space != '.')
                return false;
        }
        for (int i = 0; i < 4; i++) {
            for (int j = 0; j < 4; j++) {
                if (rooms[i][j] != correct[i])
                    return false;
            }
        }
        return true;
    }

    @Override
    public String toString() {
        // Visualizes the current RoomState. Not actually used in the implementation, but useful for debugging.
        final StringBuilder stringBuilder = new StringBuilder();
        stringBuilder.append("#############\n#");
        for (int i = 0; i < 11; i++) {
            stringBuilder.append(hallway[i]);
        }
        stringBuilder.append("#\n###");
        for (int j = 0; j < 4; j++) {
            stringBuilder.append(rooms[j][0]);
            stringBuilder.append('#');
        }
        stringBuilder.append("##\n");
        for (int i = 1; i < 4; i++) {
            stringBuilder.append("  #");
            for (int j = 0; j < 4; j++) {
                stringBuilder.append(rooms[j][i]);
                stringBuilder.append('#');
            }
            stringBuilder.append('\n');
        }
        stringBuilder.append("  #########");
        return stringBuilder.toString();
    }
}