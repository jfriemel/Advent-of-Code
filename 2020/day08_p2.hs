import Data.List ((\\))
import Data.List.Split (splitOn)
import System.Environment (getArgs)

{- Advent of Code 2020 - Day 8 - Puzzle 2 -}

main :: IO ()
main = do
  args <- getArgs
  run (head args)

run :: String -> IO ()
run file = do
  input <- readFile file
  let inputList = map (\xs -> (\[a,b] -> (a,read (b \\ "+"))) (splitOn " " xs)) $ lines input
  putStrLn $ "Result after correction: " ++ show (changeInstr 0 inputList) ++ "."

changeInstr :: Int -> [(String, Int)] -> Int
changeInstr c code
  | instr == "jmp" = case execute 0 0 (take c code ++ [("nop",val)] ++ drop (c+1) code) [] of
                       Just x  -> x
                       Nothing -> changeInstr (c+1) code
  | instr == "nop" = case execute 0 0 (take c code ++ [("jmp",val)] ++ drop (c+1) code) [] of
                       Just x  -> x
                       Nothing -> changeInstr (c+1) code
  | otherwise      = changeInstr (c+1) code
     where (instr,val) = code !! c

execute :: Int -> Int -> [(String, Int)] -> [Int] -> Maybe Int
execute c acc code prevc
  | c == length code = Just acc
  | c `elem` prevc   = Nothing
  | instr == "jmp"   = execute (c+val) acc       code (c:prevc)
  | instr == "acc"   = execute (c+1)   (acc+val) code (c:prevc)
  | instr == "nop"   = execute (c+1)   acc       code (c:prevc)
     where (instr,val) = code !! c