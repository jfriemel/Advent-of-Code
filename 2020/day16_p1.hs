import Data.List.Split    (splitOn)
import System.Environment (getArgs)

{- Advent of Code 2020 - Day 16 - Puzzle 1 -}

main :: IO ()
main = do
  args <- getArgs
  run (head args)

run :: String -> IO ()
run file = do
  input <- readFile file
  let inputList = map lines $ splitOn "\n\n" input
      rules = map parseRule (head inputList)
      invalid = findInvalid rules (concatMap (map read . splitOn ",") (tail (last inputList)))
  putStrLn $ "Invalid ticket values: " ++ show invalid ++ "."
  putStrLn $ "Sum: " ++ show (sum invalid) ++ "."

parseRule :: String -> (Int -> Bool)
parseRule str = \x -> x >= l1 && x <= h1 || x >= l2 && x <= h2
  where [[l1,h1],[l2,h2]] = map (map read . splitOn "-") $ splitOn " or " $ last $ splitOn ": " str

findInvalid :: [Int -> Bool] -> [Int] -> [Int]
findInvalid rules []     = []
findInvalid rules (x:xs)
  | any (\f -> f x) rules =     findInvalid rules xs
  | otherwise             = x : findInvalid rules xs