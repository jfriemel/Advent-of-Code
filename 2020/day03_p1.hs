import System.Environment (getArgs)

{- Advent of Code 2020 - Day 3 - Puzzle 1 -}

main = do
  args <- getArgs
  run (head args)

run file = do
  input <- readFile file
  let inputList = lines input
  putStrLn $ "Number of trees: " ++ show (countTrees 0 (3,1) inputList) ++ "."

countTrees :: Int -> (Int, Int) -> [String] -> Int
countTrees _   _     []     = 0
countTrees pos (x,y) (z:zs) = (fromEnum $ z !! pos == '#') + countTrees ((pos+x) `mod` (length z)) (x,y) (drop (y-1) zs)