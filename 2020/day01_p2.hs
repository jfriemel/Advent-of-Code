import Data.List (find, (\\))
import System.Environment (getArgs)

{- Advent of Code 2020 - Day 1 - Puzzle 2 -}

main :: IO ()
main = do
  args <- getArgs
  run (head args)

run :: String -> IO ()
run file = do
  input <- readFile file
  let inputList = map read $ lines input
  case (checkEntries inputList) of
    Just (x,y,z) -> putStrLn $ "Found triplet: " ++ (show (x,y,z)) ++ ". Product: " ++ (show (x * y * z)) ++ "."
    Nothing      -> putStrLn $ "No triplet found."

checkEntries :: (Eq a, Num a) => [a] -> Maybe (a, a, a)
checkEntries xs = find (\(x,y,z) -> x + y + z == 2020) [(x, y, z) | x <- xs, y <- xs \\ [x], z <- xs \\ [x, y]]