import Data.List.Split    (splitOn)
import System.Environment (getArgs)

{- Advent of Code 2020 - Day 22 - Puzzle 1 -}

main :: IO ()
main = do
  args <- getArgs
  run (head args)

run :: String -> IO ()
run file = do
  input <- readFile file
  let [xs,ys] = map (map read . tail . lines) (splitOn "\n\n" input)
  putStrLn $ "Winning player's score: " ++ show (play xs ys) ++ "."

play :: [Int] -> [Int] -> Int
play xs [] = calcScore xs (length xs)
play [] ys = calcScore ys (length ys)
play (x:xs) (y:ys)
  | x > y     = play (xs ++ [x,y]) ys
  | otherwise = play xs (ys ++ [y,x])

calcScore :: [Int] -> Int -> Int
calcScore []     _ = 0
calcScore (x:xs) n = n * x + calcScore xs (n-1)