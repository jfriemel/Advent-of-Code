import Data.Either        (Either)
import Data.Function      (on)
import Data.List          (foldl', nubBy)
import Data.List.Split    (splitOn)
import Text.Printf        (printf)
import System.Environment (getArgs)

{- Advent of Code 2020 - Day 14 - Puzzle 2 -}

{- > stack ghci day_14_p2_jfriemel.hs
 - > run "day_14_input_jfriemel.txt"
 -
 - Erwartetes Ergebnis: 3706820676200      -}

main :: IO ()
main = do
  args <- getArgs
  run (head args)

run :: String -> IO ()
run file = do
  input <- readFile file
  let inputList = map readInstr $ lines input
  putStrLn $ "Sum of values in memory: " ++ show (updateMem inputList "" []) ++ "."

readInstr :: String -> Either String (Int, Int)
readInstr str
  | i == "mask" = Left (head is)
  | otherwise   = Right (read (head is), read (is !! 1))
  where (i:is) = splitOn "=" $ format str
        format [] = []
        format (x:xs)
          | x == ' ' || x == ']' =       format xs
          | x == '['             = '=' : format xs
          | otherwise            =  x  : format xs

updateMem :: [Either String (Int, Int)] -> String -> [(Int, Int)] -> Int
updateMem []                       _ mem
  = foldl' (\m (_,n) -> m + n) 0 (nubBy ((==) `on` fst) mem)
updateMem (Left m'            : xs) _ mem
  = updateMem xs m' mem
updateMem (Right (addr, val) : xs) m mem
  = updateMem xs m  (map (\a -> (fromBinary a, val)) (applyMask m (toBinary addr)) ++ mem)

applyMask :: String -> String -> [String]
applyMask    []     vs  = [vs]
applyMask (m:ms) (v:vs)
  | m == 'X'  = map ('0':) (applyMask ms vs) ++ map ('1':) (applyMask ms vs)
  | m == '1'  = map ('1':) $ applyMask ms vs
  | otherwise = map (v:)   $ applyMask ms vs

toBinary :: Int -> String
toBinary = printf "%036b"

fromBinary :: String -> Int
fromBinary = foldl' (\acc c -> 2 * acc + read [c]) 0