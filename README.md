# Advent-of-Code

My personal solutions for [Advent of Code](https://adventofcode.com/), an Advent calendar with programming puzzles.

| Year | Language | Stars |
|------|----------|-------|
| 2023 | Kotlin   | 10    |
| 2022 | Python   | 35    |
| 2021 | Java     | 50    |
| 2020 | Haskell  | 44    |


## Running 2023 Solutions

For this year, I created a Gradle project and I am using Kotlin (1.9.21). Insert your input files into the `src/main/resources` directory with file names `inputDD.txt` where `DD` is the day of the month (e.g. `input04.txt` for day 4). Then, you can either run all solutions at once or specify a day by passing it as a parameter.


## Running 2022 Solutions

In 2022, I am using Python 3.11. To run my solutions, create an `input` directory next to the solutions directory and insert your input files with file names `inputDD.txt`, where `DD` is the day of the month (e.g. `input04.txt` for day 4). Then, you can run the day's Python file from the `solutions` folder. For example, for day 4 you would run `python solutions/day04.py`.


## Running 2021 Solutions

For my 2021 setup, I created a Java 17 Maven project that is partially based on [Dave Burke's Advent of Code Java template](https://github.com/dave-burke/advent-of-code-java-starter).

To be able to run my code, you first need to insert your input files into the `src/main/resources` directory with file names `inputDD.txt` where `DD` is the day of the month (e.g. `input04.txt` for day 4). Then, you can run the program with two parameters. The first parameter is the day for which you want to see the solution to your input. The second parameter is the part of the puzzle that you want to see solved (each day's puzzle is split into two parts). If you omit the second parameter, both parts' solutions of the selected day are shown. If you omit both parameters, all puzzle solutions are computed and printed.


## Running 2020 Solutions

Many of my solutions use the `Data.List.Split` package. If you are using cabal, you can install this package by executing `cabal install split`.

Each day's solution reads the input file from the path given as the program's first parameter and solves the day's puzzle for that input file. For example, to see the solution for part 2 of day 7, save your puzzle input for day 7 to a file (say `input_07.txt`), compile `day_07_p2.hs` and then run `.\day_07_p2 input_07.txt`.

You can also run the solutions in GHCi without compiling by executing `ghci day_DD_pX.hs` and then using the function `run [input]`, where `[input]` is the path to the input file.
