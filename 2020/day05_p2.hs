import Data.List (sort, foldl')
import System.Environment (getArgs)

{- Advent of Code 2020 - Day 5 - Puzzle 2 -}

main = do
  args <- getArgs
  run (head args)

run file = do
  input <- readFile file
  let sids = getSeatIDs $ lines input
  putStrLn $ "My seat ID: " ++ show (findSeatID sids (head sids)) ++ "."

findSeatID :: [Int] -> Int -> Int
findSeatID []     n = n
findSeatID (x:xs) n
  | n < x     = n
  | otherwise = findSeatID xs (n+1)

getSeatIDs :: [String] -> [Int]
getSeatIDs xs = sort $ map getSeatID xs

getSeatID :: String -> Int
getSeatID xs = 8 * getNumericValue (take 7 xs) 'B' + getNumericValue (drop 7 xs) 'R'

getNumericValue :: String -> Char -> Int
getNumericValue xs t = foldl' (\acc c -> 2 * acc + fromEnum (c == t)) 0 xs