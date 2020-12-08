import qualified Data.Vector as V
import Data.List.Split (splitOn)
import System.Environment (getArgs)

{- Advent of Code 2019 - Day 2 - Puzzle 1 -}

main :: IO ()
main = do
  args <- getArgs
  run (head args)

run :: String -> IO ()
run file = do
  input <- readFile file
  let intcode = (\vec -> vec V.// [(1,12),(2,2)]) $ V.fromList $ map (read :: String -> Int) $ splitOn "," input
  putStrLn $ "Value at position 0: " ++ show (execute 0 intcode) ++ "."

execute :: Int -> V.Vector Int -> Int
execute ip intcode
  | intcode V.! ip == 99 = intcode V.! 0
  | intcode V.! ip == 1  = execute (ip+4) (intcode V.// [(intcode V.! (ip+3), valAtAddr (ip+1) intcode + valAtAddr (ip+2) intcode)])
  | intcode V.! ip == 2  = execute (ip+4) (intcode V.// [(intcode V.! (ip+3), valAtAddr (ip+1) intcode * valAtAddr (ip+2) intcode)])

valAtAddr :: Int -> V.Vector Int -> Int
valAtAddr pt intcode = intcode V.! (intcode V.! pt)
