package de.jfriemel.aoc.days;

import de.jfriemel.aoc.Day;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class Day21 implements Day {

    int determDieVal = 0;
    int numOfDetermDieRolls = 0;

    @Override
    public String part1(List<String> input) {
        // Straightforward: Play until one player wins.
        int[] pos = {Integer.parseInt(input.get(0).substring(28))-1, Integer.parseInt(input.get(1).substring(28))-1};
        int[] score = {0, 0};

        int turn = 0;
        while (score[0] < 1000 && score[1] < 1000) {
            pos[turn] = (pos[turn] + rollDetermDie() + rollDetermDie() + rollDetermDie()) % 10;
            score[turn] += pos[turn] + 1;
            turn = (turn + 1) % 2;
        }

        return Integer.toString(numOfDetermDieRolls * Math.min(score[0], score[1]));
    }

    @Override
    public String part2(List<String> input) {
        final int pos1 = Integer.parseInt(input.get(0).substring(28)) - 1;
        final int pos2 = Integer.parseInt(input.get(1).substring(28)) - 1;
        final long[] wins = play(new HashMap<>(), pos1, pos2, 0, 0);
        return Long.toString(Math.max(wins[0], wins[1]));
    }

    private int rollDetermDie() {
        determDieVal = (determDieVal % 100) + 1;
        numOfDetermDieRolls++;
        return determDieVal;
    }

    private long[] play(final Map<String, long[]> seen, final int pos1, final int pos2,
                                                        final int score1, final int score2) {
        // Main idea: If two games have the same game state (same positions and scores), then the same player will win
        //            in both games, regardless of the previous turns/rolls.
        //            Save the game states and the number of wins you get when you start with any of those game states.
        //            This avoids having to simulate trillions of games.
        long[] answer = new long[]{0L, 0L};
        if (score1 >= 21) {
            answer[0]++;
            return answer;
        } else if (score2 >= 21) {
            answer[1]++;
            return answer;
        } else if (seen.containsKey(pos1 + "," + pos2 + "," + score1 + "," + score2)) {
            return seen.get(pos1 + "," + pos2 + "," + score1 + "," + score2);
        }

        for (int i = 1; i <= 3; i++) {
            for (int j = 1; j <= 3; j++) {
                for (int k = 1; k <= 3; k++) {
                    final int nextPos1 = (pos1 + i + j + k) % 10;
                    final int nextScore1 = score1 + nextPos1 + 1;
                    // Switch positions and scores for player 1 and 2 in recursive call to avoid having to use a
                    // variable that keeps track of the current turn.
                    final long[] nextAnswer = play(seen, pos2, nextPos1, score2, nextScore1);
                    answer[0] += nextAnswer[1];
                    answer[1] += nextAnswer[0];
                }
            }
        }

        seen.put(pos1 + "," + pos2 + "," + score1 + "," + score2, answer);
        return answer;
    }

}