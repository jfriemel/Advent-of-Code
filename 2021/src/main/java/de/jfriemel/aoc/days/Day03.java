package de.jfriemel.aoc.days;

import de.jfriemel.aoc.Day;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class Day03 implements Day {

    @Override
    public String part1(List<String> input) {
        int length = input.get(0).length();
        int numLines = input.size();
        int[] bitsSet = new int[length];
        Arrays.fill(bitsSet, 0);

        for (final String line : input) {
            for (int i = 0; i < length; i++) {
                if (line.charAt(i) == '1') {
                    bitsSet[i]++;
                }
            }
        }

        int gamma = 0;
        int epsilon = 0;
        for (int i = 0; i < length; i++) {
            if (bitsSet[i] > numLines / 2) {
                gamma = gamma * 2 + 1;
                epsilon *= 2;
            } else {
                gamma *= 2;
                epsilon = epsilon * 2 + 1;
            }
        }

        return Integer.toString(gamma * epsilon);
    }

    @Override
    public String part2(List<String> input) {
        int bitPos = 0;
        List<String> oxyList = new ArrayList<>(input);
        while (oxyList.size() > 1) {
            oxyList = prune(oxyList, bitPos, true);
            bitPos++;
        }

        bitPos = 0;
        List<String> co2List = new ArrayList<>(input);
        while (co2List.size() > 1) {
            co2List = prune(co2List, bitPos, false);
            bitPos++;
        }

        String oxyString = oxyList.get(0);
        String co2String = co2List.get(0);
        int oxy = 0;
        int co2 = 0;
        for (int i = 0; i < oxyString.length(); i++) {
            oxy = oxy * 2 + Integer.parseInt(String.valueOf(oxyString.charAt(i)));
            co2 = co2 * 2 + Integer.parseInt(String.valueOf(co2String.charAt(i)));
        }

        return Integer.toString(oxy * co2);
    }

    private List<String> prune(List<String> input, int bitPos, boolean mostCommon) {
        List<String> list0 = new ArrayList<>();
        List<String> list1 = new ArrayList<>();
        for (final String line : input) {
            if (line.charAt(bitPos) == '0') {
                list0.add(line);
            } else {
                list1.add(line);
            }
        }
        if (list1.size() >= list0.size()) {
            return mostCommon ? list1 : list0;
        } else {
            return mostCommon ? list0 : list1;
        }
    }
    
}
