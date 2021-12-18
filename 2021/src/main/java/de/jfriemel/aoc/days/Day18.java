package de.jfriemel.aoc.days;

import de.jfriemel.aoc.Day;

import java.util.Deque;
import java.util.LinkedList;
import java.util.List;

class Snailfish {
    // Data structure for a snailfish. Basically a tree.

    // The parent of the snailfish in the tree. If it is the outer snailfish (i.e. it has no parent), this is null.
    Snailfish parent;

    // The value of a regular snailfish. If it is negative, the snailfish is recognized as a pair.
    int value;

    // left and right reference the pair elements if the snailfish is a pair.
    Snailfish left;
    Snailfish right;

    Snailfish (final Snailfish parent, final int value) {
        this.parent = parent;
        this.value = value;
    }

    @Override
    public String toString() {
        if (value >= 0) {
            return Integer.toString(value);
        }
        return '[' + left.toString() + ',' + right.toString() + ']';
    }
}

public class Day18 implements Day {

    @Override
    public String part1(List<String> input) {
        Snailfish snailfish = parseSnailfish(input.get(0));
        for (final String line : input.subList(1, input.size())) {
            snailfish = add(snailfish, parseSnailfish(line));
            reduce(snailfish);
        }
        return Integer.toString(calcMagnitude(snailfish));
    }

    @Override
    public String part2(List<String> input) {
        int biggest = 0;
        // Try all possible combinations of snailfish.
        for (int i = 0; i < input.size(); i++) {
            for (int j = 0; j < input.size(); j++) {
                if (i == j) // Do not add a snailfish to itself.
                    continue;
                final Snailfish snail1 = parseSnailfish(input.get(i));
                final Snailfish snail2 = parseSnailfish(input.get(j));
                final Snailfish sum = add(snail1, snail2);
                reduce(sum);
                final int magnitude = calcMagnitude(sum);
                if (magnitude > biggest)
                    biggest = magnitude;
            }
        }
        return Long.toString(biggest);
    }

    Snailfish parseSnailfish(final String snailString) {
        // Takes a string and creates a Snailfish object corresponding to the string.
        return parseSnailfishHelper(snailString, 0, snailString.length(), null);
    }

    private Snailfish parseSnailfishHelper(final String snailString, final int start, final int end,
                                           final Snailfish parent) {
        // Recursive helper method for the snailfish parsing.
        // snailString: String for the snailfish.
        // start/end:   The bounds for the substring of snailString that defines the currently parsed snailfish.
        // parent:      Reference to the currently parsed snailfish's parent (relevant for the explosion).

        if (Character.isDigit(snailString.charAt(start))) {
            // The currently parsed snailfish is a regular number.
            // Generally, the regular number will only be one digit long but for testing purposes, the following code
            // parses numbers of any length.
            StringBuilder numberBuilder = new StringBuilder();
            int i = start;
            while (Character.isDigit(snailString.charAt(i))) {
                numberBuilder.append(snailString.charAt(i));
                i++;
            }
            return new Snailfish(parent, Integer.parseInt(numberBuilder.toString()));
        }

        Snailfish snailfish = new Snailfish(parent, -1);
        int depth = 0; // The depth int is used to determine, which [,] belong together.

        for (int i = start + 1; i < end; i++) {
            if (snailString.charAt(i) == '[') {
                depth++;
            } else if (snailString.charAt(i) == ']') {
                depth--;
            } else if (depth == 0 && snailString.charAt(i) == ',') {
                // parseSnailfishHelper is recursively called to parse the left (and right) snailfish in the pair.
                snailfish.left = parseSnailfishHelper(snailString, start + 1, i, snailfish);
                for (int j = i + 1; j < end; j++) {
                    if (snailString.charAt(j) == '[') {
                        depth++;
                    } else if (snailString.charAt(j) == ']') {
                        if (depth == 0) {
                            snailfish.right = parseSnailfishHelper(snailString, i + 1, j, snailfish);
                            break;
                        } else {
                            depth--;
                        }
                    }
                }
                break;
            }
        }
        return snailfish;
    }

    Snailfish add(final Snailfish left, final Snailfish right) {
        // Adds two snailfish without reducing.
        final Snailfish sum = new Snailfish(null, -1);
        sum.left = left;
        sum.right = right;
        left.parent = sum;
        right.parent = sum;
        return sum;
    }

    void reduce(final Snailfish snailfish) {
        // Reduces a snailfish.
        // This is generally called after two snailfish have been added.
        boolean split;
        do {
            boolean exploded;
            do {
                exploded = explode(snailfish, 0, new LinkedList<>());
            } while (exploded);
            split = split(snailfish);
        } while (split);
    }

    boolean explode(final Snailfish snailfish, final int depth, final Deque<Boolean> lastTurns) {
        // Checks whether a snailfish needs to explode and performs the explosion.
        // snailfish: The snailfish that may or may not explode.
        // depth:     The depth of the current snailfish (to determine whether it needs to explode).
        // lastTurns: The path of turns from the root to arrive at the current snailfish.
        //            true: left turn. false: right turn.

        if (snailfish.value >= 0) {
            // Reduced snailfish do not explode.
            return false;
        }

        else if (depth < 4) {
            // If depth < 4, the snailfish does not explode. Check the contained snailfish.
            lastTurns.push(true); // Left turn.
            if (explode(snailfish.left, depth + 1, new LinkedList<>(lastTurns))) {
                // Only perform one explosion at a time. If the left sub-snailfish had an explosion, return.
                return true;
            } else {
                // Only check the right sub-snailfish if the left sub-snailfish had no explosion.
                lastTurns.pop();          // Remove the previous left turn.
                lastTurns.push(false); // Right turn.
                return explode(snailfish.right, depth + 1, new LinkedList<>(lastTurns));
            }
        }

        else { // depth = 4. -> Explosion time!
            Snailfish parent = snailfish;
            Deque<Boolean> lastTurnsLeft = new LinkedList<>(lastTurns);

            // Determine the regular snailfish to the left closest to the exploding snailfish.
            while (!lastTurnsLeft.isEmpty()) {
                parent = parent.parent;

                // Loop idea: Move up the "snailfish tree" until a left turn results in a different branch than the
                //            branch of the exploding snailfish.
                //            Then make a single left turn followed by a number of right turns to end up at the regular
                //            snailfish to the left closest to the exploding snailfish.
                if (!lastTurnsLeft.pop()) {
                    Snailfish regular = parent.left;
                    while (regular.value < 0) {
                        regular = regular.right;
                    }
                    regular.value += snailfish.left.value;
                    break;
                }
            }

            parent = snailfish;
            Deque<Boolean> lastTurnsRight = new LinkedList<>(lastTurns);

            // Determine the regular snailfish to the right closest to the exploding snailfish.
            while (!lastTurnsRight.isEmpty()) {
                parent = parent.parent;

                // Loop idea: Move up the "snailfish tree" until a right turn results in a different branch than the
                //            branch of the exploding snailfish.
                //            Then make a single right turn followed by a number of left turns to end up at the regular
                //            snailfish to the right closest to the exploding snailfish.
                if (lastTurnsRight.pop()) {
                    Snailfish regular = parent.right;
                    while (regular.value < 0) {
                        regular = regular.left;
                    }
                    regular.value += snailfish.right.value;
                    break;
                }
            }

            // The snailfish becomes regular by setting its value to 0 (no longer negative).
            // The left and right objects do not need to be overwritten.
            snailfish.value = 0;
            return true;
        }
    }

    boolean split(final Snailfish snailfish) {
        // Checks whether a snailfish needs to split and performs the split.

        if (snailfish.value < 0) {
            // If snailfish.value is negative, the snailfish is a pair. Recursively apply split to the pair elements.
            if (split(snailfish.left)) {
                return true;
            } else {
                return split(snailfish.right);
            }
        }

        if (snailfish.value > 9) {
            // If snailfish.value > 9, the snailfish needs to split.
            snailfish.left = new Snailfish(snailfish, (int) Math.floor(((double) snailfish.value) / 2.0));
            snailfish.right = new Snailfish(snailfish, (int) Math.ceil(((double) snailfish.value) / 2.0));
            snailfish.value = -1; // By setting the value to -1, the snailfish gets recognized as a pair.
            return true;
        }

        return false;
    }

    int calcMagnitude(final Snailfish snailfish) {
        // Calculates the magnitude of a snailfish.
        if (snailfish.value >= 0)
            return snailfish.value;
        return 3 * calcMagnitude(snailfish.left) + 2 * calcMagnitude(snailfish.right);
    }

}