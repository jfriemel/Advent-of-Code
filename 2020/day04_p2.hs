import Data.List (filter, intersect, sort)
import Data.List.Split (splitOn)
import Data.Maybe (isJust)
import System.Environment (getArgs)
import Text.Read (readMaybe)

{- Advent of Code 2020 - Day 4 - Puzzle 2 -}

main = do
  args <- getArgs
  run (head args)

run file = do
  input <- readFile file
  let inputList = map (map (\c -> if c == '\n' then ' '; else c)) $ splitOn "\n\n" input
  putStrLn $ "Number of valid passports: " ++ show (countValids inputList) ++ "."

countValids :: [String] -> Int
countValids [] = 0
countValids (x:xs)
  | take 7 (map (take 3) passport) /= ["byr","ecl","eyr","hcl","hgt","iyr","pid"] =     countValids xs
  | and [(validations !! i) (passport !! i) | i <- [0..6]]                        = 1 + countValids xs
  | otherwise                                                                     =     countValids xs
     where passport = sort $ filter (\str -> take 3 str /= "cid") $ splitOn " " x

validations :: [String -> Bool]
validations = [validateByr, validateEcl, validateEyr, validateHcl, validateHgt, validateIyr, validatePid]

validateByr :: String -> Bool
validateByr xs = val >= 1920 && val <= 2002
  where val = (getNumValue . getValue) xs

validateEcl :: String -> Bool
validateEcl xs = getValue xs `elem` ["amb", "blu", "brn", "gry", "grn" ,"hzl" ,"oth"]

validateEyr :: String -> Bool
validateEyr xs = val >= 2020 && val <= 2030
  where val = (getNumValue . getValue) xs

validateHcl :: String -> Bool
validateHcl xs = length (val `intersect` (['a'..'f'] ++ ['0'..'9'])) == 6
  where val = (tail . getValue) xs

validateHgt :: String -> Bool
validateHgt xs
  | unit == "cm" = val >= 150 && val <= 193
  | unit == "in" = val >= 59  && val <= 76
  | otherwise    = False
     where (val, unit) = (\(x,y) -> (getNumValue x, y)) $ splitAt (length ls - 2) ls
           ls          = getValue xs

validateIyr :: String -> Bool
validateIyr xs = val >= 2010 && val <= 2020
  where val = (getNumValue . getValue) xs

validatePid :: String -> Bool
validatePid xs = length val == 9 && isJust (readMaybe val :: Maybe Int)
  where val = getValue xs

getValue :: String -> String
getValue xs = last $ splitOn ":" xs

getNumValue :: String -> Int
getNumValue xs = case readMaybe xs of
                   Just y  -> y
                   Nothing -> -1