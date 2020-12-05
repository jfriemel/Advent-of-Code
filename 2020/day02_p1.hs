import Data.List.Split (splitOn)
import System.Environment (getArgs)

{- Advent of Code 2020 - Day 2 - Puzzle 1 -}

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
countValids (x:xs) = checkValidity (getBounds bounds) (head char) 0 pwd + countValids xs
  where [bounds, char, pwd] = words x

getBounds :: String -> (Int, Int)
getBounds bounds = (read lo, read hi)
  where [lo, hi] = splitOn "-" bounds

checkValidity :: (Int, Int) -> Char -> Int -> String -> Int
checkValidity (lo, hi) _ n []
  | n < lo || n > hi = 0
  | otherwise        = 1
checkValidity (lo, hi) c n (x:xs)
  | c == x    = checkValidity (lo, hi) c (n+1) xs
  | otherwise = checkValidity (lo, hi) c n     xs