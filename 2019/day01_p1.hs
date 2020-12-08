import System.Environment (getArgs)

{- Advent of Code 2019 - Day 1 - Puzzle 1 -}

main :: IO ()
main = do
  args <- getArgs
  run (head args)

run :: String -> IO ()
run file = do
  input <- readFile file
  let inputList = map read $ lines input
  putStrLn $ "Fuel required ignoring fuel: " ++ show (fuelRequired inputList) ++ "."

fuelRequired :: [Int] -> Int
fuelRequired xs = sum $ map (\n -> n `div` 3 - 2) xs