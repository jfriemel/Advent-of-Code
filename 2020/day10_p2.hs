import Data.List (sort)
import System.Environment (getArgs)

{- Advent of Code 2020 - Day 10 - Puzzle 2 -}

main :: IO ()
main = do
  args <- getArgs
  run (head args)

run :: String -> IO ()
run file = do
  input <- readFile file
  let inputList = map read $ lines input
  putStrLn $ "Number of adapter arrangements: " ++ show (numOfArrs (differences (0:sort inputList)) []) ++ "."

numOfArrs :: [Int] -> [Int] -> Int
numOfArrs []     ys = partNum ys 0
numOfArrs (3:xs) ys = numOfArrs xs [] * partNum ys 0
numOfArrs (x:xs) ys = numOfArrs xs (x:ys)

partNum :: [Int] -> Int -> Int
partNum []     y = if y <= 3 then 1 else 0
partNum (x:xs) 0 = partNum xs x
partNum (x:xs) y
  | y <= 3    = partNum xs (x+y) + partNum xs x
  | otherwise = 0

differences :: [Int] -> [Int]
differences [_]      = [3]
differences (x:y:ys) = (y-x) : differences (y:ys)