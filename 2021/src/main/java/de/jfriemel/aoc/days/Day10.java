package de.jfriemel.aoc.days;

import de.jfriemel.aoc.Day;

import java.util.*;

public class Day10 implements Day {

    @Override
    public String part1(List<String> input) {
        return Integer.toString(pruneList(input, null));
    }

    @Override
    public String part2(List<String> input) {
        List<String> pruned = new ArrayList<>();
        pruneList(input, pruned);
        List<Long> scores = new ArrayList<>();
        for (final String line : pruned) {
            long score = 0;
            Deque<Character> openingBrackets = new LinkedList<>();
            for (final char bracket : line.toCharArray()) {
                if (isOpening(bracket)) {
                    openingBrackets.push(bracket);
                } else {
                    openingBrackets.pop();
                }
            }
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
        scores = scores.stream().sorted().toList();
        return Long.toString(scores.get(scores.size() / 2));
    }

    private int pruneList(List<String> input, List<String> pruned) {
        int sum = 0;
        lineLoop:
        for (final String line : input) {
            Deque<Character> openingBrackets = new LinkedList<>();
            for (final char bracket : line.toCharArray()) {
                if (isOpening(bracket)) {
                    openingBrackets.push(bracket);
                } else {
                    final char opening = openingBrackets.pop();
                    final int bracketSum = opening + bracket;
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
            if (pruned != null) {
                pruned.add(line);
            }
        }
        return sum;
    }

    private boolean isOpening(final char bracket) {
        return bracket == '(' || bracket == '[' || bracket == '{' || bracket == '<';
    }

}