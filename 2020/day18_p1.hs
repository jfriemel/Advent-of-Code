import Data.Char          (digitToInt, isDigit)
import System.Environment (getArgs)

{- Advent of Code 2020 - Day 18 - Puzzle 1 -}

main :: IO ()
main = do
  args <- getArgs
  run (head args)

run :: String -> IO ()
run file = do
  input <- readFile file
  let inputList = map (filter (/=' ')) $ lines input
  putStrLn $ "Sum of homework results: " ++ show (sum (map evaluateLine inputList)) ++ "."

evaluateLine :: String -> Int
evaluateLine str = evaluatePolish [] $ shuntingYard str []

evaluatePolish :: [Int] -> String -> Int
evaluatePolish (a:_) []     = a
evaluatePolish as    (p:ps)
  | isDigit p = evaluatePolish (digitToInt p : as)  ps
  | otherwise = evaluatePolish (toOp p a1 a2 : as') ps
  where ([a1,a2],as') = splitAt 2 as

shuntingYard :: String -> String -> String
shuntingYard []     []     = []
shuntingYard []     (o:os) = o : shuntingYard [] os
shuntingYard (i:is) os
  | isDigit i     = i : shuntingYard is os
  | i == '('      = shuntingYard is (i:os)
  | i == ')'      = (\(_:os',out) -> out ++ shuntingYard is    os')  (popStack os [])
  | i `elem` "+*" = (\(  os',out) -> out ++ shuntingYard is (i:os')) (popStack os [])

popStack :: String -> String -> (String, String)
popStack []     out = ([], reverse out)
popStack (o:os) out
  | o == '('  = (o:os, reverse out)
  | otherwise = popStack os (o:out)

toOp :: (Num a) => Char -> (a -> a -> a)
toOp '+' = (+)
toOp '*' = (*)