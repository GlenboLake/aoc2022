module Day08 where
import AOC (solve)
import Data.Char (digitToInt)
import Data.List (transpose)

type Map = [[Int]]

isVisible :: Map -> Int -> Int -> Bool
isVisible m r c = do
  let height = m!!r!!c
  let west = take c (m !! r)
  let east = drop (c+1) (m !! r)
  let north = take r ((transpose m) !! c)
  let south = drop (r+1) ((transpose m) !! c)
  let visibleOver trees = all (<height) trees
  any visibleOver [west,east,north,south]

parseInput :: String -> Map
parseInput input = map (map digitToInt) $ lines input

part1 :: String -> Int
part1 input = do
   let m = parseInput input
   let numRows = length m
   let numCols = length $ m!!1
   let border = numRows*numCols - ((numRows-2)*(numCols-2))
   let coords = [(r,c) | r <- [1..(numRows-2)], c <- [1..(numCols-2)]]
   let visible = length $ filter (\ (r,c) -> isVisible m r c) coords
   visible + border

-- Alternate version of takeWhile that includes the
-- first item that doesn't satisfy the predicate
takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' p (x:xs)
  | p x       = x : takeWhile' p xs
  | otherwise = [x]

scoreTree m r c = do
  let height = m!!r!!c
  let west = reverse $ take c (m !! r)
  let east = drop (c+1) (m !! r)
  let north = reverse $ take r ((transpose m) !! c)
  let south = drop (r+1) ((transpose m) !! c)
  let look trees = length $ takeWhile' (<height) trees
  product $ map look [west, east, north, south]

part2 :: String -> Int
part2 input = do
   let m = parseInput input
   let numRows = length m
   let numCols = length $ m!!1
   let coords = [(r,c) | r <- [0..(numRows-1)], c <- [0..(numCols-1)]]
   maximum [scoreTree m r c | (r,c) <- coords]

main input = do
  putStr "Part 1: "
  solve part1 input
  putStr "Part 2: "
  solve part2 input
