import           Data.List          ((\\), intersect, filter)
import           Data.List.Split    (splitOn)
import qualified Data.Map as M      (Map, fromList, toList, unionsWith)
import           System.Environment (getArgs)

{- Advent of Code 2020 - Day 21 - Puzzle 1 -}

main :: IO ()
main = do
  args <- getArgs
  run (head args)

run :: String -> IO ()
run file = do
  input <- readFile file
  let inputList   = lines input
      ingrLines   = map parseLine inputList
      allIngr     = concatMap (splitOn " " . head . splitOn " (") inputList
      ingrMatches = allMatches (M.toList (M.unionsWith intersect ingrLines)) []
      noAllergs   = filter (`notElem` map snd ingrMatches) allIngr
  putStrLn $ "Number of ingredients not containing allergens: " ++ show (length noAllergs) ++ "."

allMatches :: [(String, [String])] -> [(String, String)] -> [(String, String)]
allMatches [] ms = ms
allMatches xs ms = allMatches pruned (newMatches ++ ms)
  where
    newMatches = matchIteration xs
    pruned     = filter (not . null . snd) (map (\(allerg,is) -> (allerg,is \\ map snd newMatches)) xs)

matchIteration :: [(String, [String])] -> [(String, String)]
matchIteration []                   = []
matchIteration ((allerg,[ingr]):xs) = (allerg,ingr) : matchIteration xs
matchIteration (_              :xs) =                 matchIteration xs

parseLine :: String -> M.Map String [String]
parseLine str = M.fromList (map (\s -> (s, splitOn " " ls)) (splitOn ", " (init rs)))
  where [ls,rs] = splitOn " (contains " str