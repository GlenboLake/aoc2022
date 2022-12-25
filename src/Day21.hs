module Day21 where
import AOC (solve)
import Data.List (intercalate)
import Data.Maybe (fromJust)

type Op = String
type Name = String

data Monkey = Number Int | Math Op Name Name

instance Show Monkey where
  show (Number a)    = "Yelling " ++ (show a)
  show (Math op a b) = intercalate " " [op++"ing", a, "and", b]

type MonkeyMap = [(String,Monkey)]

parseInput input = map parseMonkey $ lines input

parseMonkey :: String -> (String, Monkey)
parseMonkey line = case words $ filter (/=':') line of
  [name, num]      -> (name, Number (read num))
  [name, a, op, b] -> (name, Math op a b)

parseOp :: String -> (Integral a => a -> a -> a)
parseOp "+" = (+)
parseOp "-" = (-)
parseOp "*" = (*)
parseOp "/" = div

evalMonkey :: Name -> MonkeyMap -> Int
evalMonkey name ms = case lookup name ms of
  Just (Number a)    -> a
  Just (Math op a b) -> (parseOp op) (evalMonkey a ms) (evalMonkey b ms)

expandMonkey :: Name -> MonkeyMap -> String
expandMonkey name ms =
  if name == "humn" then "x"
  else case lookup name ms of
    Just (Number a)    -> show a
    Just (Math op a b) -> "(" ++ (expandMonkey a ms) ++ op ++ (expandMonkey b ms) ++ ")"

part1 input = evalMonkey "root" $ parseInput input

part2 input = do
  let ms = parseInput input
  let (Math _ left right) = fromJust $ lookup "root" ms
  (expandMonkey left ms) ++ "-" ++ (expandMonkey right ms) 

main input = do
  solve part1 input
  solve part2 input