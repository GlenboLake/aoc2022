module Day09 where
import AOC (solve)
import Data.List (nub)

type Point = (Int, Int)
type Rope = (Point, Point)

initRope :: Rope
initRope = ((0,0),(0,0))

data Dir = L | R | U | D
  deriving (Show)

parseInput input = concat $ map parseLine $ lines input

parseLine line =
  let (d:n:_) = words line
      dir = case d of "L" -> L
                      "R" -> R
                      "U" -> U
                      "D" -> D
      dist = read n :: Int
  in replicate dist dir

touches :: Point -> Point -> Bool
(x1,y1) `touches` (x2,y2) = abs (x1-x2) <= 1 && abs (y1-y2) <= 1

move :: Rope -> Dir -> Rope
move ((hx,hy), t) dir = do
  let newHead = case dir of L -> (hx-1, hy)
                            R -> (hx+1, hy)
                            U -> (hx, hy+1)
                            D -> (hx, hy-1)
  let newTail = if newHead `touches` t then t else (hx, hy)
  (newHead, newTail)

followPath :: Rope -> [Dir] -> [Rope]
followPath rope [] = [rope]
followPath rope (d:ds) = rope:(followPath (move rope d) ds)

debug input = do
  let path = parseInput input
  followPath initRope path


part1 input = length $ nub $ map snd $ followPath initRope $ parseInput input
part2 input = ""

main input = do
  putStr "Part 1: "
  solve part1 input
  putStr "Part 2: "
  solve part2 input