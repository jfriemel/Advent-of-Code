import Data.List ((\\))
import Data.List.Split (splitOn)
import System.Environment (getArgs)

{- Advent of Code 2020 - Day 8 - Puzzle 1 -}

main :: IO ()
main = do
  args <- getArgs
  run (head args)

run :: String -> IO ()
run file = do
  input <- readFile file
  let inputList = map (\xs -> (\[a,b] -> (a,read (b \\ "+"))) (splitOn " " xs)) $ lines input
  putStrLn $ "Result after abortion: " ++ show (execute 0 0 inputList []) ++ "."

execute :: Int -> Int -> [(String, Int)] -> [Int] -> Int
execute ip acc code previp
  | ip `elem` previp = acc
  | instr == "jmp"   = execute (ip+val) acc       code (ip:previp)
  | instr == "acc"   = execute (ip+1)   (acc+val) code (ip:previp)
  | instr == "nop"   = execute (ip+1)   acc       code (ip:previp)
  where (instr, val) = code !! ip