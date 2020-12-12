import System.Environment (getArgs)

{- Advent of Code 2020 - Day 12 - Puzzle 2 -}

data Direction = North | East | South | West deriving (Bounded, Enum, Eq)

main :: IO ()
main = do
  args <- getArgs
  run (head args)

run :: String -> IO ()
run file = do
  input <- readFile file
  let inputList = map (\str -> (head str, read (tail str))) $ lines input
  let (x,y) = moveShip (10,1) (0,0) inputList
  putStrLn $ "Final ship position: " ++ show (x,y) ++ "."
  putStrLn $ "Manhattan distance:  " ++ show (abs x + abs y) ++ "."

moveShip :: (Int, Int) -> (Int, Int) -> [(Char, Int)] -> (Int, Int)
moveShip (wx,wy) (x,y) []             = (x,y)
moveShip (wx,wy) (x,y) (('L',deg):xs) = moveShip (rotate (wx,wy) (360-deg))    (x,     y     ) xs
moveShip (wx,wy) (x,y) (('R',deg):xs) = moveShip (rotate (wx,wy)      deg )    (x,     y     ) xs
moveShip (wx,wy) (x,y) (('F',d)  :xs) = moveShip         (wx,wy)               (x+d*wx,y+d*wy) xs
moveShip (wx,wy) (x,y) ((c,  d)  :xs) = moveShip (moveWp (wx,wy) (getDir c) d) (x,     y     ) xs
  where getDir 'N' = North
        getDir 'E' = East
        getDir 'S' = South
        getDir 'W' = West

rotate :: (Int, Int) -> Int -> (Int, Int)
rotate (wx,wy) 0   = (wx,wy)
rotate (wx,wy) deg = rotate (wy,-wx) (deg-90)

moveWp :: (Int, Int) -> Direction -> Int -> (Int, Int)
moveWp (wx,wy) North d = (wx,wy+d)
moveWp (wx,wy) East  d = (wx+d,wy)
moveWp (wx,wy) South d = (wx,wy-d)
moveWp (wx,wy) West  d = (wx-d,wy)