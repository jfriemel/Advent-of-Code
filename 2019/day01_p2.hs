import System.Environment (getArgs)

{- Advent of Code 2019 - Day 1 - Puzzle 2 -}

main :: IO ()
main = do
  args <- getArgs
  run (head args)

run :: String -> IO ()
run file = do
  input <- readFile file
  let inputList = map read $ lines input
  putStrLn $ "Total fuel required: " ++ show (fuelRequired inputList) ++ "."

fuelRequired :: [Int] -> Int
fuelRequired xs = sum $ map (\n -> additionalFuel (n `div` 3 - 2)) xs

additionalFuel :: Int -> Int
additionalFuel x
  | x > 0     = x + additionalFuel (x `div` 3 - 2)
  | otherwise = 0