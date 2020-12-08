import Data.List (filter, intersect, nub)
import Data.List.Split (splitOn)
import System.Environment (getArgs)

{- Advent of Code 2020 - Day 6 - Puzzle 2 -}

main :: IO ()
main = do
  args <- getArgs
  run (head args)

run :: String -> IO ()
run file = do
  input <- readFile file
  let inputList = map (splitOn "\n") $ splitOn "\n\n" input
  putStrLn $ "Number of 'yes': " ++ show (countYes inputList) ++ "."

countYes :: [[String]] -> Int
countYes []          = 0
countYes ((x:xs):ys) = countYesGroup xs x + countYes ys

countYesGroup :: [String] -> String -> Int
countYesGroup []     ys = length ys
countYesGroup (x:xs) ys = countYesGroup xs (x `intersect` ys)