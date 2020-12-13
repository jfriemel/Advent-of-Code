import Data.Function      (on)
import Data.List          (minimumBy)
import Data.List.Split    (splitOn)
import System.Environment (getArgs)

{- Advent of Code 2020 - Day 13 - Puzzle 1 -}

main :: IO ()
main = do
  args <- getArgs
  run (head args)

run :: String -> IO ()
run file = do
  input <- readFile file
  let inputList = lines input
  let me = read $ head inputList
  let ids = map read $ filter (/= "x") $ splitOn "," (inputList !! 1)
  let earliest = minimumBy (compare `on` snd) (map (departure me) ids)
  putStrLn $ "Bus with earliest departure time: " ++ show earliest ++ "."
  putStrLn $ "Result: " ++ show (fst earliest * (snd earliest - me)) ++ "."

departure :: Int -> Int -> (Int, Int)
departure me id = (id, id * (me `div` id + 1))