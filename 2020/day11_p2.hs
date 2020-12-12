import qualified Data.Vector as V
import           System.Environment (getArgs)

{- Advent of Code 2020 - Day 11 - Puzzle 2 -}

main :: IO ()
main = do
  args <- getArgs
  run (head args)

run :: String -> IO ()
run file = do
  input <- readFile file
  let grid = V.fromList $ map V.fromList $ lines input
  let final = iterateGrid grid (nextGrid grid [] 0)
  putStrLn $ "Number of occupied seats: " ++ show (countOccupied (V.toList final)) ++ "."

iterateGrid :: V.Vector (V.Vector Char) -> V.Vector (V.Vector Char) -> V.Vector (V.Vector Char)
iterateGrid curr prev
  | curr == prev = curr
  | otherwise    = iterateGrid (nextGrid curr [] 0) curr

nextGrid :: V.Vector (V.Vector Char) -> [(Int, V.Vector Char)] -> Int -> V.Vector (V.Vector Char)
nextGrid grid ys n
  | n == V.length grid
      = grid V.// ys
  | otherwise
      = nextGrid grid ((n,V.fromList [nextState grid i n | i <- [0..(V.length (grid V.! n)-1)]]):ys) (n+1)

nextState :: V.Vector (V.Vector Char) -> Int -> Int -> Char
nextState grid x y
  | val == '.' = '.'
  | val == 'L' = if null (getNeighbors grid x y) then '#' else 'L'
  | val == '#' = if length (getNeighbors grid x y) >= 5 then 'L' else '#'
  where val = (grid V.! y) V.! x

countOccupied :: [V.Vector Char] -> Int
countOccupied xs = V.length $ V.filter (=='#') $ V.concat xs

getNeighbors :: V.Vector (V.Vector Char) -> Int -> Int -> String
getNeighbors grid x y = filter (=='#') [getNearest grid (x+dx) (y+dy) (dx,dy) | dx <- [-1,0,1], dy <- [-1,0,1], dx /= 0 || dy /= 0]

getNearest :: V.Vector (V.Vector Char) -> Int -> Int -> (Int, Int) -> Char
getNearest grid x y (dx,dy)
  | x < 0 || y < 0 || x == V.length (V.head grid) || y == V.length grid
      = '.'
  | val /= '.'
      = val
  | otherwise
      = getNearest grid (x+dx) (y+dy) (dx,dy)
  where val = (grid V.! y) V.! x