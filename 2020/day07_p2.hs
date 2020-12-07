import Data.List (filter, isSuffixOf, nub)
import Data.List.Split (splitOn)
import Data.Text (pack, replace, unpack)
import System.Environment (getArgs)
import qualified Data.Map as Map (Map, fromList, lookup)

{- Advent of Code 2020 - Day 7 - Puzzle 2 -}

main :: IO ()
main = do
  args <- getArgs
  run (head args)

run :: String -> IO ()
run file = do
  input <- readFile file
  let inputList = lines input
  putStrLn $ "Number of bags the 'shiny gold' bag needs to contain: " ++ show ((numberOfBags "shiny gold" $ bagMap inputList) - 1) ++ "."

numberOfBags :: String -> Map.Map String [(Int, String)] -> Int
numberOfBags bag mp = case Map.lookup bag mp of
                        Just xs -> 1 + sum (map (\(n,b) -> n * numberOfBags b mp) xs)
                        Nothing -> 1

bagMap :: [String] -> Map.Map String [(Int, String)]
bagMap = Map.fromList . map simplifyLine . filter (\str -> not (isSuffixOf "no other bags." str))

simplifyLine :: String -> (String, [(Int, String)])
simplifyLine str = (a,map (\(x:_:ys) -> (read [x],ys)) (splitOn ", " b))
  where [a,b] = splitOn " contain " $ replaceStr " bag" "" $ replaceStr " bags" "" $ init str

replaceStr :: String -> String -> String -> String
replaceStr x y str = unpack $ replace (pack x) (pack y) (pack str)