import System.Environment (getArgs)

{- Advent of Code 2020 - Day 3 - Puzzle 2 -}

main = do
  args <- getArgs
  run (head args)

run file = do
  input <- readFile file
  let inputList = lines input
  let numberOfTrees = map (\s -> (s, countTrees 0 s inputList)) [(1,1),(3,1),(5,1),(7,1),(1,2)]
  putStrLn $ "Number of trees per slope: " ++ show numberOfTrees ++ "."
  putStrLn $ "Product: " ++ show (foldr (\(_, x) y -> x*y) 1 numberOfTrees) ++ "."

countTrees :: Int -> (Int, Int) -> [String] -> Int
countTrees _   _     []     = 0
countTrees pos (x,y) (z:zs) = (fromEnum $ z !! pos == '#') + countTrees ((pos+x) `mod` (length z)) (x,y) (drop (y-1) zs)