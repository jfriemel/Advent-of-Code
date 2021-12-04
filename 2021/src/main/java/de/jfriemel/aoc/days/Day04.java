package de.jfriemel.aoc.days;

import de.jfriemel.aoc.Day;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.LinkedList;
import java.util.List;

public class Day04 implements Day {

    @Override
    public String part1(List<String> input) {
        return gameLoop(input, false);
    }

    @Override
    public String part2(List<String> input) {
        return gameLoop(input, true);
    }

    private String gameLoop(final List<String> input, final boolean lastWinner) {
        List<Integer> numbers = new ArrayList<>();
        List<int[][]> boardsNums = new ArrayList<>();
        List<boolean[][]> boardsHits = new ArrayList<>();
        initialize(input, numbers, boardsNums, boardsHits);

        for (int number : numbers) {
            int row;
            int col = 0;

            for (int i = 0; i < boardsNums.size(); i++) {
                int[][] boardNums = boardsNums.get(i);
                boolean[][] boardHits = boardsHits.get(i);

                rowLoop:
                for (row = 0; row < boardNums.length; row++) {
                    for (col = 0; col < boardNums[row].length; col++) {
                        if (boardNums[row][col] == number) {
                            boardHits[row][col] = true;
                            break rowLoop;
                        }
                    }
                }

                if (row < boardNums.length && col < boardNums.length && checkBoard(boardHits, row, col)) {
                    if (lastWinner && boardsNums.size() > 1) {
                        boardsNums.remove(boardNums);
                        boardsHits.remove(boardHits);
                        i = -1; // Make sure not to skip a board after the removal.
                    } else {
                        return Integer.toString(calcBoardScore(boardNums, boardHits, number));
                    }
                }
            }

        }
        return "No winning board.";
    }

    private void initialize(List<String> input, List<Integer> numbers, List<int[][]> boardsNums,
                            List<boolean[][]> boardsHits) {
        String inputStr = String.join("\n", input);
        String[] boards = inputStr.split("\n\n");
        String[] numbersArr = boards[0].split(",");
        for (String numberStr : numbersArr) {
            numbers.add(Integer.parseInt(numberStr));
        }

        for (int i = 1; i < boards.length; i++) {
            String[] boardLines = boards[i].split("\n");
            int[][] boardNums = new int[boardLines.length][];
            boolean[][] boardHits = new boolean[boardLines.length][];
            for (int j = 0; j < boardLines.length; j++) {
                String[] boardElems = boardLines[j].trim().replaceAll(" +", " ").split(" ");
                boardNums[j] = new int[boardElems.length];
                boardHits[j] = new boolean[boardElems.length];
                for (int k = 0; k < boardElems.length; k++) {
                    boardNums[j][k] = Integer.parseInt(boardElems[k]);
                }
            }
            boardsNums.add(boardNums);
            boardsHits.add(boardHits);
        }
    }

    private boolean checkBoard(final boolean[][] boardHits, final int row, final int col) {
        int n = boardHits.length;
        boolean winner = true;
        for (boolean[] boardHit : boardHits) {
            winner &= boardHit[col];
        }
        if (winner) {
            return true;
        }
        winner = true;
        for (int k = 0; k < n; k++) {
            winner &= boardHits[row][k];
        }
        return winner;
    }

    private int calcBoardScore(final int[][] boardNums, final boolean[][] boardHits, final int number) {
        int score = 0;
        for (int j = 0; j < boardNums.length; j++) {
            for (int k = 0; k < boardNums[j].length; k++) {
                if (!boardHits[j][k]) {
                    score += boardNums[j][k];
                }
            }
        }
        return score * number;
    }

}
