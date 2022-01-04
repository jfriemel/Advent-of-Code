package de.jfriemel.aoc.days;

import de.jfriemel.aoc.Day;

import java.util.ArrayList;
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
        // Plays the game until either any board has won (lastWinner = false) or all boards have won (lastWinner = true).
        final List<Integer>     numbers    = new ArrayList<>(); // List of numbers that are called in the game.
        final List<int[][]>     boardsNums = new ArrayList<>(); // List of all game boards (int grids).
        final List<boolean[][]> boardsHits = new ArrayList<>(); // List of all hits on all boards (boolean grids).
        initialize(input, numbers, boardsNums, boardsHits);

        for (final int number : numbers) {
            int row;
            int col = 0;

            for (int i = 0; i < boardsNums.size(); i++) {
                final int[][]     boardNums = boardsNums.get(i);
                final boolean[][] boardHits = boardsHits.get(i);

                rowLoop: // Find the number that has been called on the board, mark the corresponding field as hit.
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
                        // Remove the winning board from the list since it no longer needs to be played.
                        boardsNums.remove(boardNums);
                        boardsHits.remove(boardHits);
                        i = -1; // Make sure not to skip a board after the removal.
                    } else {
                        // If lastWinner = true, the last board has won, the game is done.
                        // If lastWinner = false, the first board has won, the game is done.
                        return Integer.toString(calcBoardScore(boardNums, boardHits, number));
                    }
                }
            }

        }
        return "No winning board.";
    }

    private void initialize(final List<String> input, final List<Integer> numbers, final List<int[][]> boardsNums,
                            final List<boolean[][]> boardsHits) {
        // Parses the input and fills the lists given as parameters with the appropriate values.
        final String inputStr = String.join("\n", input);
        final String[] boards = inputStr.split("\n\n");
        final String[] numbersArr = boards[0].split(",");
        for (final String numberStr : numbersArr) {
            numbers.add(Integer.parseInt(numberStr));
        }

        for (int i = 1; i < boards.length; i++) {
            final String[]    boardLines = boards[i].split("\n");
            final int[][]     boardNums  = new int[boardLines.length][];
            final boolean[][] boardHits  = new boolean[boardLines.length][];

            for (int j = 0; j < boardLines.length; j++) {
                final String[] boardElems = boardLines[j].trim().replaceAll(" +", " ").split(" ");
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
        // Checks if a board is a winning board. Only checks the specified row and column which are the row and column
        // of the most recently called number.
        boolean winner = true;
        for (boolean[] boardHit : boardHits)
            winner &= boardHit[col];

        if (winner)
            return true;

        winner = true;
        for (int k = 0; k < boardHits.length; k++)
            winner &= boardHits[row][k];

        return winner;
    }

    private int calcBoardScore(final int[][] boardNums, final boolean[][] boardHits, final int number) {
        // Calculates the score of a winning board based on the hits on the board and the most recently called number.
        int score = 0;
        for (int j = 0; j < boardNums.length; j++) {
            for (int k = 0; k < boardNums[j].length; k++) {
                if (!boardHits[j][k])
                    score += boardNums[j][k];
            }
        }
        return score * number;
    }

}