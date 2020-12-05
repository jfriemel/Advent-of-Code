import Data.List (foldl')
import System.Environment (getArgs)

{- Advent of Code 2020 - Day 5 - Puzzle 1 -}

main = do
  args <- getArgs
  run (head args)

run file = do
  input <- readFile file
  let inputList = lines input
  putStrLn $ "Highest seat ID: " ++ show (maximum $ map getSeatID inputList) ++ "."

getSeatID :: String -> Int
getSeatID xs = 8 * getNumericValue (take 7 xs) 'B' + getNumericValue (drop 7 xs) 'R'

getNumericValue :: String -> Char -> Int
getNumericValue xs hi = foldl (\acc c -> 2 * acc + fromEnum (c == hi)) 0 xs