package de.jfriemel.aoc.days;

import de.jfriemel.aoc.Day;

import java.util.HashMap;
import java.util.List;

public class Day08 implements Day {

    @Override
    public String part1(List<String> input) {
        final List<String[]> outList = input.stream().map(s -> s.split(" \\| ")[1].split(" ")).toList();

        int sum = 0;
        for (final String[] out : outList) {
            for (final String symbol : out) {
                if (symbol.length() == 2 || symbol.length() == 3 || symbol.length() == 4 || symbol.length() == 7)
                    sum++;
            }
        }

        return Integer.toString(sum);
    }

    @Override
    public String part2(List<String> input) {
        // Idea: Count how often each symbol appears in the input (the ten signal patterns on the left of |).
        //       Then, for each digit in the output part: Add up the previously determined counts for each symbol.
        //       The resulting sum will be characteristic for each possible digit (0 to 9).

        final HashMap<Integer, Integer> digitLookup = new HashMap<>();
        // The digitLookup maps contains the characteristic sums for all 10 digits:
        digitLookup.put(42, 0);
        digitLookup.put(17, 1);
        digitLookup.put(34, 2);
        digitLookup.put(39, 3);
        digitLookup.put(30, 4);
        digitLookup.put(37, 5);
        digitLookup.put(41, 6);
        digitLookup.put(25, 7);
        digitLookup.put(49, 8);
        digitLookup.put(45, 9);

        final List<String[]> sepList = input  .stream().map(s -> s.split(" \\| ")).toList();
        final List<String[]> inList  = sepList.stream().map(s -> s[0].split(" ")) .toList();
        final List<String[]> outList = sepList.stream().map(s -> s[1].split(" ")) .toList();

        int outputSum = 0;
        for (int i = 0; i < inList.size(); i++) {
            final String[] inLine = inList.get(i);
            final int[] charCounts = new int[]{0, 0, 0, 0, 0, 0, 0};
            for (final String symbol : inLine) {
                for (final char single : symbol.toCharArray()) {
                    charCounts[single - 'a']++;
                }
            }

            final String[] outLine = outList.get(i);
            final StringBuilder numString = new StringBuilder();
            for (final String symbol : outLine) {
                int sum = 0;
                for (final char single : symbol.toCharArray()) {
                    sum += charCounts[single - 'a'];
                }
                numString.append(digitLookup.get(sum));
            }

            outputSum += Integer.parseInt(numString.toString());
        }

        return Integer.toString(outputSum);
    }

}