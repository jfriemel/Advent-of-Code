import System.Environment (getArgs)

{- Advent of Code 2020 - Day 12 - Puzzle 1 -}

data Direction = North | East | South | West deriving (Bounded, Enum, Eq)

main :: IO ()
main = do
  args <- getArgs
  run (head args)

run :: String -> IO ()
run file = do
  input <- readFile file
  let inputList = map (\str -> (head str, read (tail str))) $ lines input
  let (x,y) = moveShip (0,0) East inputList
  putStrLn $ "Final ship position: " ++ show (x,y) ++ "."
  putStrLn $ "Manhattan distance:  " ++ show (abs x + abs y) ++ "."

moveShip :: (Int, Int) -> Direction -> [(Char, Int)] -> (Int, Int)
moveShip (x,y) _   []             = (x,y)
moveShip (x,y) dir (('L',deg):xs) = moveShip       (x,y)               (nextDir dir (360-deg)) xs
moveShip (x,y) dir (('R',deg):xs) = moveShip       (x,y)               (nextDir dir      deg ) xs
moveShip (x,y) dir (('F',d)  :xs) = moveShip (move (x,y) dir        d)          dir            xs
moveShip (x,y) dir ((c,d)    :xs) = moveShip (move (x,y) (getDir c) d)          dir            xs
  where getDir 'N' = North
        getDir 'E' = East
        getDir 'S' = South
        getDir 'W' = West

nextDir :: Direction -> Int -> Direction
nextDir dir 0 = dir
nextDir dir deg
  | dir == maxBound = nextDir minBound   (deg-90)
  | otherwise       = nextDir (succ dir) (deg-90)

move :: (Int, Int) -> Direction -> Int -> (Int, Int)
move (x,y) North d = (x,y+d)
move (x,y) East  d = (x+d,y)
move (x,y) South d = (x,y-d)
move (x,y) West  d = (x-d,y)