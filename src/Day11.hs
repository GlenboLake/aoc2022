module Day11 where
import AOC (solve)
import Data.Char (isDigit)
import Data.List (intercalate, sort)
import Data.List.Split (splitOn)

data Monkey = Monkey { name :: Int
                     , items :: [Int]
                     , inspection :: Inspection
                     , testValue :: Int
                     , trueTarget :: Int
                     , falseTarget :: Int
                     , nInspections :: Int
}

type Inspection = (Char, Int)

-- Detailed show for debugging parse
--instance Show Monkey where
--    show m = "<Monkey " ++ show (name m) ++ ": "
--              ++ (intercalate "," $ map show (items m))
--              ++ "; " ++ ((fst $ inspection m) : (show $ snd $ inspection m))
--              ++ "; throws to " ++ (show $ trueTarget m) ++ " or " ++ (show $ falseTarget m)
--              ++ " with " ++ (show $ testValue m)
--              ++ ">"
-- Simple show for debugging output
instance Show Monkey where
  show m = "<Monkey " ++ show (name m) ++ ": "
           ++ (intercalate ", " $ map show (items m))
           ++ " (" ++ (show (nInspections m)) ++ " inspections)"

parseMonkey :: String -> Monkey
parseMonkey monkey = do
  let (id':items':op:test':true':false':_) = lines monkey
  let startingItems = (map read $ splitOn ", " $ snd $ splitAt 18 items')::[Int]
  let testValue = read (last $ words test')::Int
  let inspectionOperator = head $ words op !! 4
  let inspectionOperand = last $ words op
  let inspection = if (inspectionOperator, inspectionOperand) == ('*', "old") then ('^', 2)
                   else (inspectionOperator, (read inspectionOperand)::Int)
  Monkey { name = read (filter isDigit id')::Int
         , items = startingItems
         , inspection = inspection
         , testValue = testValue
         , trueTarget = read (last $ words true')::Int
         , falseTarget = read ( last $ words false')::Int
         , nInspections = 0
         }

parseInput input = map parseMonkey $ splitOn "\n\n" input

inspectionFunc :: Monkey -> Int -> Int -> (Int -> Int)
inspectionFunc m r b = (`mod` b) . (`div` r) . (f $ inspection m) where
  f ('+', i) = (+) i
  f ('*', i) = (*) i
  f ('^', 2) = flip (^) 2

takeTurn :: [Monkey] -> Int -> Int -> Int -> [Monkey]
takeTurn ms i divMod base = do
  let items = inspect (ms !! i) divMod base
  giveItems ms items i

-- Inspection for each monkey returns a list
-- of monkeys to give items to, along with their new worry values
inspect :: Monkey -> Int -> Int -> [(Int, Int)]
inspect m divMod base = do
  let f = inspectionFunc m divMod base
  let newWorryValues = map f $ items m
  let test n = if n `mod` (testValue m) == 0 then (trueTarget m) else (falseTarget m)
  let targets = map test newWorryValues
  zip targets newWorryValues

giveItems :: [Monkey] -> [(Int, Int)] -> Int -> [Monkey]
giveItems ms is thrower = map updated ms
  where
    updated m = m { items = if (name m) == thrower then []
                            else newItemList m
                  , nInspections = nInspections m + if (name m) == thrower then length $ items m
                                                    else 0
      }
    newItemList m = items m ++ [n | (i,n) <- is, i == name m]

multipleTurns :: [Monkey] -> Int -> Int -> [Int] -> [Monkey]
multipleTurns ms _ _ [] = ms
multipleTurns ms divMod base (i:is) = multipleTurns (takeTurn ms i divMod base) divMod base is

doRound :: Int -> Int -> [Monkey] -> [Monkey]
doRound divMod base ms = multipleTurns ms divMod base [0..length ms-1]

part1 input = do
  let ms = parseInput input
  let base = foldl lcm 1 $ map testValue ms
  let result = iterate (doRound 3 base) ms !! 20
  product $ take 2 $ reverse $ sort $ map nInspections result

part2 input = do
  let ms = parseInput input
  let base = foldl lcm 1 $ map testValue ms
  let result = iterate (doRound 1 base) ms !! 10000
  product $ take 2 $ reverse $ sort $ map nInspections result

main input = do
  solve part1 input
  solve part2 input
