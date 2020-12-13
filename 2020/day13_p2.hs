import Data.List.Split    (splitOn)
import System.Environment (getArgs)

{- Advent of Code 2020 - Day 13 - Puzzle 2 -}

main :: IO ()
main = do
  args <- getArgs
  run (head args)

run :: String -> IO ()
run file = do
  input <- readFile file
  let inputList = splitOn "," $ lines input !! 1
  let congr = [(fromIntegral (-a) `mod` p, p) | a <- [0 .. length inputList - 1], let ps = inputList !! a, ps /= "x", let p = read ps]
  putStrLn $ "Gold coin timestamp: " ++ show (chineseRemainder congr) ++ "."

departure :: (Integral a) => a -> a -> (a, a)
departure me id = (\xs -> (id, head xs)) $ filter (>= me) [id * x | x <- [1 ..]]

chineseRemainder :: (Integral a) => [(a, a)] -> a
chineseRemainder [(a,p)]              = a `mod` p
chineseRemainder ((a1,p1):(a2,p2):xs) = chineseRemainder (((a2*c*p1 + a1*d*p2) `mod` p1p2, p1p2):xs)
  where p1p2  = p1 * p2
        (c,d) = extEuclid p1 p2

extEuclid :: (Integral a) => a -> a -> (a, a)
extEuclid e f = euclidHelper f e 1 0 0 1

euclidHelper :: (Integral a) => a -> a -> a -> a -> a -> a -> (a, a)
euclidHelper _  0  c  _  d  _  = (d,c)
euclidHelper a1 a2 c1 c2 d1 d2 = euclidHelper a2 (a1-q*a2) c2 (c1-q*c2) d2 (d1-q*d2)
  where q = a1 `div` a2