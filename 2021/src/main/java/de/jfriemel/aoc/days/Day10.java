package de.jfriemel.aoc.days;

import de.jfriemel.aoc.Day;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.Deque;
import java.util.LinkedList;
import java.util.List;

public class Day10 implements Day {

    @Override
    public String part1(List<String> input) {
        return Integer.toString(pruneList(input, null));
    }

    @Override
    public String part2(List<String> input) {
        final List<String> pruned = new ArrayList<>();
        pruneList(input, pruned);

        // Now, only look at the uncorrupted lines.
        final List<Long> scores = new ArrayList<>();
        for (final String line : pruned) {
            long score = 0;
            final Deque<Character> openingBrackets = new LinkedList<>();
            for (final char bracket : line.toCharArray()) {
                if (isOpening(bracket))
                    openingBrackets.push(bracket);
                else
                    openingBrackets.pop();
            }

            // Look at all opening brackets that have not been closed. Use them to compute the final score.
            while (!openingBrackets.isEmpty()) {
                score *= 5;
                switch (openingBrackets.pop()) {
                    case '(' -> score += 1;
                    case '[' -> score += 2;
                    case '{' -> score += 3;
                    case '<' -> score += 4;
                }
            }

            scores.add(score);
        }

        scores.sort(Comparator.naturalOrder());
        return Long.toString(scores.get(scores.size() / 2));
    }

    private int pruneList(final List<String> input, final List<String> pruned) {
        // Computes the total syntax error score (return value) for part 1 and creates a list of uncorrupted lines
        // (parameter 'pruned') for part 2.

        int sum = 0;

        lineLoop:
        for (final String line : input) {
            // Keep track of opening brackets using a stack.
            final Deque<Character> openingBrackets = new LinkedList<>();

            for (final char bracket : line.toCharArray()) {
                if (isOpening(bracket)) {
                    openingBrackets.push(bracket);
                } else {
                    final char opening = openingBrackets.pop();
                    final int bracketSum = opening + bracket;
                    // Easy way to check whether the brackets align: Compare their char sum.
                    if (bracketSum != '(' + ')' && bracketSum != '[' + ']'
                     && bracketSum != '{' + '}' && bracketSum != '<' + '>') {
                        switch (bracket) {
                            case ')' -> sum += 3;
                            case ']' -> sum += 57;
                            case '}' -> sum += 1197;
                            case '>' -> sum += 25137;
                        }
                        continue lineLoop;
                    }
                }
            }

            if (pruned != null) // Only prune for part 2.
                pruned.add(line);
        }

        return sum;
    }

    private boolean isOpening(final char bracket) {
        return bracket == '(' || bracket == '[' || bracket == '{' || bracket == '<';
    }

}