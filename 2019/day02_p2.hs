import qualified Data.Vector as V
import Data.List.Split (splitOn)
import System.Environment (getArgs)

{- Advent of Code 2019 - Day 2 - Puzzle 2 -}

main :: IO ()
main = do
  args <- getArgs
  run (head args)

run :: String -> IO ()
run file = do
  input <- readFile file
  let intcode = (\vec -> vec V.// [(1,12),(2,2)]) $ V.fromList $ map (read :: String -> Int) $ splitOn "," input
  putStrLn $ "Valid input: " ++ show (tryInputs intcode [(a,b) | a <- [0..99], b <- [0..99]]) ++ "."

tryInputs :: V.Vector Int -> [(Int, Int)] -> Int
tryInputs intcode ((a,b):xs)
  | execute 0 (intcode V.// [(1,a),(2,b)]) == 19690720 = 100 * a + b
  | otherwise                                          = tryInputs intcode xs

execute :: Int -> V.Vector Int -> Int
execute ip intcode
  | intcode V.! ip == 99 = intcode V.! 0
  | intcode V.! ip == 1  = execute (ip+4) (intcode V.// [(intcode V.! (ip+3), valAtAddr (ip+1) intcode + valAtAddr (ip+2) intcode)])
  | intcode V.! ip == 2  = execute (ip+4) (intcode V.// [(intcode V.! (ip+3), valAtAddr (ip+1) intcode * valAtAddr (ip+2) intcode)])

valAtAddr :: Int -> V.Vector Int -> Int
valAtAddr pt intcode = intcode V.! (intcode V.! pt)
