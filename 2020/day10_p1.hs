import Data.List (sort)
import System.Environment (getArgs)

{- Advent of Code 2020 - Day 10 - Puzzle 1 -}

main :: IO ()
main = do
  args <- getArgs
  run (head args)

run :: String -> IO ()
run file = do
  input <- readFile file
  let inputList = map read $ lines input
  let joltDiff = countJoltDiff (0:sort inputList) 0 0
  putStrLn $ "Number of 1/3 jolt differences: " ++ show joltDiff ++ "."
  putStrLn $ "Product: " ++ show (uncurry (*) joltDiff) ++ "."

countJoltDiff :: [Int] -> Int -> Int -> (Int, Int)
countJoltDiff [_]      uno tre = (uno, tre+1)
countJoltDiff (x:y:ys) uno tre
  | y - x == 1 = countJoltDiff (y:ys) (uno+1) tre
  | y - x == 3 = countJoltDiff (y:ys) uno (tre+1)
  | otherwise  = countJoltDiff (y:ys) uno     tre