import qualified Data.Vector as V
import           System.Environment (getArgs)

{- Advent of Code 2020 - Day 10 - Puzzle 1 -}

main :: IO ()
main = do
  args <- getArgs
  run (head args)

run :: String -> IO ()
run file = do
  input <- readFile file
  let inputList = map (\str -> V.fromList ('.' : str ++ ".")) $ lines input
  let len = length $ head inputList
  let grid = V.fromList $ V.replicate len '.' : inputList ++ [V.replicate len '.']
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
  | val == '#' = if length (getNeighbors grid x y) >= 4 then 'L' else '#'
  where val = (grid V.! y) V.! x

countOccupied :: [V.Vector Char] -> Int
countOccupied xs = V.length $ V.filter (=='#') $ V.concat xs

getNeighbors :: V.Vector (V.Vector Char) -> Int -> Int -> String
getNeighbors grid x y = filter (=='#') [(grid V.! j) V.! i | i <- [x-1,x,x+1], j <- [y-1,y,y+1], i /= x || j /= y]