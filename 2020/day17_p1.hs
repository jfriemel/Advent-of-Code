import Data.Map           (fromListWith, toList)
import System.Environment (getArgs)

{- Advent of Code 2020 - Day 17 - Puzzle 1 -}

type PocketDimension = [(Int, Int, Int)]

main :: IO ()
main = do
  args <- getArgs
  run (head args)

run :: String -> IO ()
run file = do
  input <- readFile file
  let inputDim = parseDimension $ lines input
  putStrLn $ "Number of active cubes: " ++ show (length (cycleDimension inputDim 6)) ++ "."

parseDimension :: [String] -> PocketDimension
parseDimension inp = [(x,y,1) | y <- [0 .. length inp - 1], x <- [0 .. length (inp !! y) - 1], (inp !! y) !! x == '#']

cycleDimension :: PocketDimension -> Int -> PocketDimension
cycleDimension dim 0 = dim
cycleDimension dim n = cycleDimension (cycleOnce (activeNeighborCounts dim) dim) (n-1)

cycleOnce :: [((Int, Int, Int), Int)] -> PocketDimension -> PocketDimension
cycleOnce []     dim = []
cycleOnce ((p,c):cs) dim
  | c == 3 || p `elem` dim && c == 2 = p : cycleOnce cs dim
  | otherwise                        =     cycleOnce cs dim

activeNeighborCounts :: PocketDimension -> [((Int, Int, Int), Int)]
activeNeighborCounts dim = toList (fromListWith (+) [(coords, 1) | coords <- concatMap (getNeighbors dim) dim])

getNeighbors :: PocketDimension -> (Int, Int, Int) -> PocketDimension
getNeighbors dim (x,y,z) = [(i,j,k) | i <- [x-1,x,x+1],
                                      j <- [y-1,y,y+1],
                                      k <- [z-1,z,z+1],
                                      i /= x || j /= y || k /= z]