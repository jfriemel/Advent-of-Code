import Data.List (filter, sort)
import Data.List.Split (splitOn)
import System.Environment (getArgs)
import Text.Read (readMaybe)

{- Advent of Code 2020 - Day 4 - Puzzle 1 -}

main :: IO ()
main = do
  args <- getArgs
  run (head args)

run :: String -> IO ()
run file = do
  input <- readFile file
  let inputList = map (map (\c -> if c == '\n' then ' '; else c)) $ splitOn "\n\n" input
  putStrLn $ "Number of valid passports: " ++ show (countValids inputList) ++ "."

countValids :: [String] -> Int
countValids [] = 0
countValids (x:xs)
  | map (take 3) passport == ["byr","ecl","eyr","hcl","hgt","iyr","pid"] = 1 + countValids xs
  | otherwise                                                            =     countValids xs
     where passport = sort $ filter (\str -> take 3 str /= "cid") $ splitOn " " x