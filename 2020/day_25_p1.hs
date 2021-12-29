import System.Environment (getArgs)

{- Advent of Code 2020 - Day 25 - Puzzle 1 -}

main :: IO ()
main = do
  args <- getArgs
  run (head args)

run :: String -> IO ()
run file = do
  input <- readFile file
  let [keyC,keyD] = map read $ lines input
  let lsizeC = findLoopSize 1 keyC 0
  let privKey = transform 1 keyD lsizeC
  putStrLn $ "Private key: " ++ show privKey ++ "."

findLoopSize :: Int -> Int -> Int -> Int
findLoopSize acc key lsize
  | acc == key = lsize
  | otherwise  = findLoopSize ((acc*7) `mod` 20201227) key (lsize+1)

transform :: Int -> Int -> Int -> Int
transform acc _    0     = acc
transform acc snum lsize = transform ((acc*snum) `mod` 20201227) snum (lsize-1)