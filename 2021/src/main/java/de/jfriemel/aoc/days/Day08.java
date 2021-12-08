package de.jfriemel.aoc.days;

import de.jfriemel.aoc.Day;

import java.util.HashMap;
import java.util.List;

public class Day08 implements Day {

    @Override
    public String part1(List<String> input) {
        List<String[]> outList = input.stream().map(s -> s.split(" \\| ")[1].split(" ")).toList();
        int sum = 0;
        for (String[] out : outList) {
            for (String symbol : out) {
                char[] chars = symbol.toCharArray();
                if (chars.length == 2 || chars.length == 3 || chars.length == 4 || chars.length == 7) {
                    sum++;
                }
            }
        }
        return Integer.toString(sum);
    }

    @Override
    public String part2(List<String> input) {
        HashMap<Integer, Integer> digitLookup = new HashMap<>();
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

        final List<String[]> sepList = input.stream().map(s -> s.split(" \\| ")).toList();
        final List<String[]> inList = sepList.stream().map(s -> s[0].split(" ")).toList();
        final List<String[]> outList = sepList.stream().map(s -> s[1].split(" ")).toList();

        int outputSum = 0;
        for (int i = 0; i < inList.size(); i++) {
            final String[] inLine = inList.get(i);
            final int[] charCounts = new int[]{0, 0, 0, 0, 0, 0, 0};
            for (String symbol : inLine) {
                for (char single : symbol.toCharArray()) {
                    charCounts[single - 'a']++;
                }
            }

            final String[] outLine = outList.get(i);
            StringBuilder numString = new StringBuilder();
            for (String symbol : outLine) {
                int sum = 0;
                for (char single : symbol.toCharArray()) {
                    sum += charCounts[single - 'a'];
                }
                numString.append(digitLookup.get(sum));
            }

            outputSum += Integer.parseInt(numString.toString());
        }
        return Integer.toString(outputSum);
    }

}