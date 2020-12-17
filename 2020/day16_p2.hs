import Data.Function      (on)
import Data.List          ((\\), sortBy, transpose)
import Data.List.Split    (splitOn)
import System.Environment (getArgs)

{- Advent of Code 2020 - Day 16 - Puzzle 2 -}

main :: IO ()
main = do
  args <- getArgs
  run (head args)

run :: String -> IO ()
run file = do
  input <- readFile file
  let inputList = map lines $ splitOn "\n\n" input
      rules = map parseRule (head inputList)
      myTicket = map read $ splitOn "," $ (inputList !! 1) !! 1
      columns = transpose $ filterTickets rules $ map (map read . splitOn ",") $ (tail . last) inputList
      mappings = getMappings (cycle (zip [0..] rules)) columns []
  putStrLn $ "Rule columns:                " ++ show mappings ++ "."
  putStrLn $ "Product of departure values: " ++ show (result myTicket (take 6 mappings)) ++ "."

result :: [Int] -> [Int] -> Int
result ticket ruleInds = product [ticket !! i | i <- ruleInds]

parseRule :: String -> (Int -> Bool)
parseRule str = \x -> x >= l1 && x <= h1 || x >= l2 && x <= h2
  where [[l1,h1],[l2,h2]] = map (map read . splitOn "-") $ splitOn " or " $ last $ splitOn ": " str

getMappings :: [(Int, Int -> Bool)] -> [[Int]] -> [(Int, Int)] -> [Int]
getMappings ((nr,r):rs) ts ms
  | length ms == 20 = map snd $ sortBy (compare `on` fst) ms
  | m >= 0          = getMappings rs ts ((nr,m):ms)
  | otherwise       = getMappings rs ts ms
  where m = getMapping r ts (map snd ms)

getMapping :: (Int -> Bool) -> [[Int]] -> [Int] -> Int
getMapping rule ts ms
  | length cand == 1 = head cand
  | otherwise        = -1
  where cand = [n | n <- [0 .. length ts - 1] \\ ms, all rule (ts !! n)]

filterTickets :: [Int -> Bool] -> [[Int]] -> [[Int]]
filterTickets _     []     = []
filterTickets rules (x:xs)
  | null (findInvalid rules x) = x : filterTickets rules xs
  | otherwise                  =     filterTickets rules xs

findInvalid :: [Int -> Bool] -> [Int] -> [Int]
findInvalid rules []     = []
findInvalid rules (x:xs)
  | any (\f -> f x) rules =     findInvalid rules xs
  | otherwise             = x : findInvalid rules xs