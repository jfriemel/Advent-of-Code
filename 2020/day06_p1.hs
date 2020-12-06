import Data.List (filter, nub)
import Data.List.Split (splitOn)
import System.Environment (getArgs)

{- Advent of Code 2020 - Day 6 - Puzzle 1 -}

main :: IO ()
main = do
  args <- getArgs
  run (head args)

run :: String -> IO ()
run file = do
  input <- readFile file
  let inputList = map (filter (/= '\n')) $ splitOn "\n\n" input
  putStrLn $ "Number of 'yes': " ++ show (countYes inputList) ++ "."

countYes :: [String] -> Int
countYes []     = 0
countYes (x:xs) = (length . nub) x + countYes xs