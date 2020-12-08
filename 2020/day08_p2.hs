import Data.List ((\\))
import System.Environment (getArgs)

{- Advent of Code 2020 - Day 8 - Puzzle 2 -}

main :: IO ()
main = do
  args <- getArgs
  run (head args)

run :: String -> IO ()
run file = do
  input <- readFile file
  let inputList = map ((\[a,b] -> (a, read (b \\ "+"))) . words) $ lines input
  putStrLn $ "Result after correction: " ++ show (executeCorrect 0 0 inputList []) ++ "."

executeCorrect :: Int -> Int -> [(String, Int)] -> [Int] -> Int
executeCorrect ip acc code previp
  | instr == "acc" = executeCorrect (ip+1) (acc+val) code (ip:previp)
  | instr == "jmp" = 
      case execute ip acc (take ip code ++ ("nop",val) : drop (ip+1) code) previp of
        Just x  -> x
        Nothing -> executeCorrect (ip+val) acc code (ip:previp)
  | instr == "nop" =
      case execute ip acc (take ip code ++ ("jmp",val) : drop (ip+1) code) previp of
        Just x  -> x
        Nothing -> executeCorrect (ip+1)   acc code (ip:previp)
  where (instr, val) = code !! ip

execute :: Int -> Int -> [(String, Int)] -> [Int] -> Maybe Int
execute ip acc code previp
  | ip == length code = Just acc
  | ip `elem` previp  = Nothing
  | instr == "jmp"    = execute (ip+val) acc       code (ip:previp)
  | instr == "acc"    = execute (ip+1)   (acc+val) code (ip:previp)
  | instr == "nop"    = execute (ip+1)   acc       code (ip:previp)
  where (instr, val)  = code !! ip