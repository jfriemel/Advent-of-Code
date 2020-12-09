import Data.List ((\\))
import System.Environment (getArgs)

{- Advent of Code 2020 - Day 9 - Puzzle 1 -}

main :: IO ()
main = do
  args <- getArgs
  run (head args)

run :: String -> IO ()
run file = do
  input <- readFile file
  let inputList = map read $ lines input
  putStrLn $ "Rogue number: " ++ show (findRogue (take 25 inputList) (drop 25 inputList)) ++ "."

findRogue :: [Int] -> [Int] -> Int
findRogue pt@(p:ps) (x:xs)
  | x `elem` [a + b | a <- pt, b <- pt \\ [a]] = findRogue (ps ++ [x]) xs
  | otherwise                                  = x