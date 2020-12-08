import Data.List (find, (\\))
import System.Environment (getArgs)

{- Advent of Code 2020 - Day 1 - Puzzle 1 -}

main :: IO ()
main = do
  args <- getArgs
  run (head args)

run :: String -> IO ()
run file = do
  input <- readFile file
  let inputList = map read $ lines input
  case checkEntries inputList of
    Just (x,y) -> putStrLn $ "Pair found: " ++ show (x,y) ++ ". Product: " ++ show (x * y) ++ "."
    Nothing    -> putStrLn "No pair found."

checkEntries :: (Eq a, Num a) => [a] -> Maybe (a, a)
checkEntries xs = find (\(x,y) -> x + y == 2020) [(x, y) | x <- xs, y <- xs \\ [x]]