module Day09 where
import AOC (solve)
import Data.List (nub)

type Point = (Int, Int)
type Rope = [Point]

newRope :: Int -> Rope
newRope size = replicate size (0,0)

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

sign :: (Ord a, Num a) => a -> a
sign a = if a>0 then 1 else if a<0 then -1 else 0

-- Logic for making one point move, diagonally if necessary, to touch another
moveToTouch :: Point -> Point -> Point
moveToTouch (px,py) (tx,ty) =
  if (px,py) `touches` (tx,ty)
    then (px,py)
    else let (mx,my) = (sign $ tx-px, sign $ ty-py)
         in moveToTouch (px+mx,py+my) (tx,ty)

-- Move a point in a cardinal direction
moveTo :: Point -> Dir -> Point
(px,py) `moveTo` dir =
  case dir of
    L -> (px-1, py)
    R -> (px+1, py)
    U -> (px, py+1)
    D -> (px, py-1)

catchup :: [Point] -> Point -> [Point]
catchup [] _ = []
catchup (p:ps) target =
  let newHead = moveToTouch p target
  in newHead:(catchup ps newHead)

move :: Rope -> Dir -> Rope
move (p:ps) dir =
  let newHead = p `moveTo` dir
  in newHead:(catchup ps newHead)

followPath :: Rope -> [Dir] -> [Rope]
followPath rope [] = [rope]
followPath rope (d:ds) = rope:(followPath (move rope d) ds)

runDay ropeLength input = length $ nub $ map last $ followPath (newRope ropeLength) $ parseInput input

part1 input = runDay 2 input
part2 input = runDay 10 input

main input = do
  putStr "Part 1: "
  solve part1 input
  putStr "Part 2: "
  solve part2 input