import           Data.Function      (on)
import           Data.List          ((\\), intercalate, intersect, filter, sortBy)
import           Data.List.Split    (splitOn)
import qualified Data.Map as M      (Map, fromList, toList, unionsWith)
import           System.Environment (getArgs)

{- Advent of Code 2020 - Day 21 - Puzzle 2 -}

main :: IO ()
main = do
  args <- getArgs
  run (head args)

run :: String -> IO ()
run file = do
  input <- readFile file
  let ingrLines   = map parseLine (lines input)
      ingrMatches = allMatches (M.toList (M.unionsWith intersect ingrLines)) []
  putStrLn $ "Dangerous ingredients: " ++ intercalate "," (map snd (sortBy (compare `on` fst) ingrMatches)) ++ "."

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