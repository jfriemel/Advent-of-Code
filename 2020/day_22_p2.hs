import Data.List.Split    (splitOn)
import System.Environment (getArgs)

{- Advent of Code 2020 - Day 22 - Puzzle 2 -}

main :: IO ()
main = do
  args <- getArgs
  run (head args)

run :: String -> IO ()
run file = do
  input <- readFile file
  let [xs,ys] = map (map read . tail . lines) (splitOn "\n\n" input)
  let ws = fst $ play [] xs ys
  putStrLn $ "Winning player's score: " ++ show (calcScore ws (length ws)) ++ "."

play :: [([Int], [Int])] -> [Int] -> [Int] -> ([Int], Bool)
play _ xs [] = (xs, True)
play _ [] ys = (ys, False)
play prev xt@(x:xs) yt@(y:ys)
  | (xt,yt) `elem` prev
      = (xt, True)
  | x <= length xs && y <= length ys
      = if snd (play [] (take x xs) (take y ys))
          then play nprev (xs ++ [x,y]) ys
          else play nprev xs (ys ++ [y,x])
  | x > y
      = play nprev (xs ++ [x,y]) ys
  | otherwise
      = play nprev xs (ys ++ [y,x])
  where nprev = (xt,yt) : prev

calcScore :: [Int] -> Int -> Int
calcScore []     _ = 0
calcScore (x:xs) n = n * x + calcScore xs (n-1)