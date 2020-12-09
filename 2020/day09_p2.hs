import Data.List ((\\))
import System.Environment (getArgs)

{- Advent of Code 2020 - Day 9 - Puzzle 2 -}

main :: IO ()
main = do
  args <- getArgs
  run (head args)

run :: String -> IO ()
run file = do
  input <- readFile file
  let inputList = map read $ lines input
  let rogue = findRogue (take 25 inputList) (drop 25 inputList)
  let contiguous = findContiguous (take 2 inputList) rogue (drop 2 inputList)
  putStrLn $ "Contiguous range: " ++ show contiguous ++ "."
  putStrLn $ "Sum of smallest and biggest number: " ++ show (minimum contiguous + maximum contiguous) ++ "."

findRogue :: [Int] -> [Int] -> Int
findRogue pt@(p:ps) (x:xs)
  | x `elem` [a + b | a <- pt, b <- pt \\ [a]] = findRogue (ps ++ [x]) xs
  | otherwise                                  = x

findContiguous :: [Int] -> Int -> [Int] -> [Int]
findContiguous ct@(_:cs) r (x:xs)
  | null cs      = findContiguous  []         r (x:xs)
  | sum  ct <  r = findContiguous (ct ++ [x]) r    xs
  | sum  ct == r = ct
  | sum  ct >  r = findContiguous  cs         r (x:xs)