import Data.List.Split (splitOn)
import System.Environment (getArgs)

{- Advent of Code 2020 - Day 2 - Puzzle 2 -}

main :: IO ()
main = do
  args <- getArgs
  run (head args)

run :: String -> IO ()
run file = do
  input <- readFile file
  let inputList = lines input
  putStrLn $ "Number of valid passwords: " ++ show (countValids inputList) ++ "."

countValids :: [String] -> Int
countValids []     = 0
countValids (x:xs) = checkValidity (getPos pos) (head char) pwd + countValids xs
  where [pos, char, pwd] = words x

getPos :: String -> (Int, Int)
getPos pos = (read a - 1, read b - 1)
  where [a, b] = splitOn "-" pos

checkValidity :: (Int, Int) -> Char -> String -> Int
checkValidity (a, b) c xs
  | xs !! a == c && xs !! b /= c = 1
  | xs !! a /= c && xs !! b == c = 1
  | otherwise                    = 0