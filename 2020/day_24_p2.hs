import           Data.List          (foldl')
import qualified Data.Map as M
import qualified Data.Set as S
import           System.Environment (getArgs)

{- Advent of Code 2020 - Day 24 - Puzzle 2 -}

data Direction = NE | E | SE | SW | W | NW deriving (Enum)
type Tile = (Int, Int, Int)
type Vector = (Int, Int, Int)

main :: IO ()
main = do
  args <- getArgs
  run (head args)

run :: String -> IO ()
run file = do
  input <- readFile file
  let directions = map parseLine $ lines input
  putStrLn $ "Number of black tiles after 100 days: " ++ show (S.size (iterateGrid 100 (blackTiles directions S.empty))) ++ "."

iterateGrid :: Int -> S.Set Tile -> S.Set Tile
iterateGrid 0 set = set
iterateGrid n set = iterateGrid (n-1) (nextGrid (neighborCounts (S.toList set) M.empty) set [])

nextGrid :: [(Tile, Int)] -> S.Set Tile -> [Tile] -> S.Set Tile
nextGrid []         _   ts = S.fromList ts
nextGrid ((t,n):ns) set ts
  | n == 2 || n == 1 && t `S.member` set = nextGrid ns set (t:ts)
  | otherwise                            = nextGrid ns set    ts

blackTiles :: [[Direction]] -> S.Set Tile -> S.Set Tile
blackTiles []       set = set
blackTiles (ds:dss) set
  | tile `S.member` set = blackTiles dss (S.delete tile set)
  | otherwise           = blackTiles dss (S.insert tile set)
  where tile = move ds

move :: [Direction] -> Tile
move = foldl' (\t d -> moveTo t (dirVector d)) (0,0,0)

moveTo :: Tile -> Vector -> Tile
moveTo (x,y,z) (dx,dy,dz) = (x+dx,y+dy,z+dz)

neighborCounts :: [Tile] -> M.Map Tile Int -> [(Tile, Int)]
neighborCounts []     mp = M.toList mp
neighborCounts (t:ts) mp = neighborCounts ts (M.unionWith (+) mp (M.fromList [(moveTo t (dirVector d), 1) | d <- [NE .. NW]]))

dirVector :: Direction -> Vector
dirVector NE = ( 1, 0,-1)
dirVector  E = ( 1,-1, 0)
dirVector SE = ( 0,-1, 1)
dirVector SW = (-1, 0, 1)
dirVector  W = (-1, 1, 0)
dirVector NW = ( 0, 1,-1)

parseLine :: String -> [Direction]
parseLine []           = []
parseLine (    'e':xs) =  E : parseLine xs
parseLine (    'w':xs) =  W : parseLine xs
parseLine ('n':'e':xs) = NE : parseLine xs
parseLine ('n':'w':xs) = NW : parseLine xs
parseLine ('s':'e':xs) = SE : parseLine xs
parseLine ('s':'w':xs) = SW : parseLine xs