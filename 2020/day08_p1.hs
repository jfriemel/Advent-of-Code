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
execute c acc code prevc
  | c `elem` prevc = acc
  | instr == "jmp" = execute (c+val) acc       code (c:prevc)
  | instr == "acc" = execute (c+1)   (acc+val) code (c:prevc)
  | instr == "nop" = execute (c+1)   acc       code (c:prevc)
     where (instr,val) = code !! c