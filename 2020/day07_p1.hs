import Data.List (filter, isSuffixOf, nub)
import Data.List.Split (splitOn)
import Data.Text (pack, replace, unpack)
import System.Environment (getArgs)
import qualified Data.Map as Map (Map, fromListWith, lookup)

{- Advent of Code 2020 - Day 7 - Puzzle 1 -}

main :: IO ()
main = do
  args <- getArgs
  run (head args)

run :: String -> IO ()
run file = do
  input <- readFile file
  let inputList = lines input
  let revmap = revBagMap inputList
  putStrLn $ "Number of bags containing the 'shiny gold' bag: " ++ show (countContainments "shiny gold" "shiny gold" $ revBagMap inputList) ++ "."

countContainments :: String -> String -> Map.Map String [(Int, String)] -> Int
countContainments bag iter mp = length $ nub $ tail $ findContainments bag iter mp

findContainments :: String -> String -> Map.Map String [(Int, String)] -> [String]
findContainments bag iter mp = case Map.lookup iter mp of
                                 Just xs -> iter : concat (map (\(_,b) -> findContainments bag b mp) xs)
                                 Nothing -> [iter]

revBagMap :: [String] -> Map.Map String [(Int, String)]
revBagMap xs = Map.fromListWith (++) $ reverseList $ map simplifyLine $ filter (\str -> not (isSuffixOf "no other bags." str)) xs

reverseList :: [(String, [(Int, String)])] -> [(String, [(Int, String)])]
reverseList [] = []
reverseList ((bag,[]):bs)         = reverseList bs
reverseList ((bag,((n,x):xs)):bs) = (x,[(n,bag)]) : reverseList ((bag,xs):bs)

simplifyLine :: String -> (String, [(Int, String)])
simplifyLine str = (a,map (\(x:_:ys) -> (read [x],ys)) (splitOn ", " b))
  where [a,b] = splitOn " contain " $ replaceStr " bag" "" $ replaceStr " bags" "" $ init str

replaceStr :: String -> String -> String -> String
replaceStr x y str = unpack $ replace (pack x) (pack y) (pack str)