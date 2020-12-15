import qualified Data.IntMap as IM  (IntMap, fromList, insert, lookup)
import           Data.List.Split    (splitOn)
import           Data.Maybe         (fromMaybe)
import           System.Environment (getArgs)

{- Advent of Code 2020 - Day 15 - Puzzle 1 -}

main :: IO ()
main = do
  args <- getArgs
  run (head args)

run :: String -> IO ()
run file = do
  input <- readFile file
  let inputList = map read $ splitOn "," input
      len = length inputList
      n = 2020
  putStrLn $ show n ++ "th number: " ++ show (play n len (last inputList) (IM.fromList (zip (init inputList) [1..]))) ++ "."

play :: Int -> Int -> Int -> IM.IntMap Int -> Int
play n i l im 
  | i == n    = l
  | otherwise = play n (i+1) next $! IM.insert l i im
  where next = i - fromMaybe i (IM.lookup l im)