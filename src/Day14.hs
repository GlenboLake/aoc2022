module Day14 where
import AOC (solve)
import Data.List (all, nub)
import Data.List.Split (splitOn)

type Coord = (Int, Int)

rock = '#'
sand = 'o'
air = '.'
src = '+'

source :: Coord
source = (500, 0)

data Grain = Falling Coord | Settled Coord | Abyss deriving (Show)

pointsBetween :: Coord -> Coord -> [Coord]
pointsBetween a@(x1,y1) b@(x2,y2)
  | x1 == x2 && y1 < y2 = map ( (x1,) ) [y1..y2]
  | x1 == x2 && y1 > y2 = map ( (x1,) ) [y2..y1]
  | y1 == y2 && x1 < x2 = map ( (,y1) ) [x1..x2]
  | y1 == y2 && x1 > x2 = map ( (,y1) ) [x2..x1]
  | otherwise           = error $ (show a) ++ " -> " ++ (show b) ++ " is not a straight line"

readCoord :: String -> Coord
readCoord c = read ("("++c++")") :: Coord

parseInput input = nub $ concat $ map parsePath $ lines input

parsePath path = do
  let coords = map readCoord $ splitOn " -> " path
  let pairs = zip coords (tail coords)
  nub $ concat $ map (uncurry pointsBetween) pairs

-- Add a floor based on the current rocks
addFloor :: [Coord] -> [Coord]
addFloor rs = let y = (maximum $ map snd rs) + 2
  -- Since sand falls as a 45-degree angle, the farthest it can go is +=y. Add a buffer of two.
  in rs ++ map (,y) (map (+500) [-(y+2)..y+2])

inAbyss :: [Coord] -> Coord -> Bool
inAbyss cs (_,y) = y > (maximum $ map snd cs)

showCave :: [Coord] -> [Coord] -> IO ()
showCave rocks sands =
  let leftBound = minimum $ map fst (rocks ++ sands)
      rightBound = maximum $ map fst (rocks ++ sands)
      bottomBound = maximum $ map snd (rocks ++ sands)
      getChar c = if c `elem` rocks then rock
        else if c `elem` sands then sand
        else if c == source then src
        else air
      showRow y = map (\ x -> getChar (x,y) ) [leftBound..rightBound]
  in putStr $ unlines $ map showRow [0..bottomBound]

dropSand :: [Coord] -> Grain
dropSand cs = followDroppedSand cs (Falling source)

followDroppedSand :: [Coord] -> Grain -> Grain
followDroppedSand _ (Settled c) = Settled c
followDroppedSand cs (Falling c@(x,y))
  | inAbyss cs c           = Abyss
  | (x,y+1) `notElem` cs   = followDroppedSand cs (Falling (x,y+1))
  | (x-1,y+1) `notElem` cs = followDroppedSand cs (Falling (x-1,y+1))
  | (x+1,y+1) `notElem` cs = followDroppedSand cs (Falling (x+1,y+1))
  | c `elem` cs            = Abyss
  | otherwise              = Settled c

collectSand :: [Coord] -> [Coord] -> [Coord]
collectSand rocks sands = case dropSand (rocks ++ sands) of
  Abyss     -> sands
  Settled s -> collectSand rocks (s:sands)

sandRange height = [ (x,y) | y <- [0..height], x <- [500-y..500+y]]

part1 input = length $ collectSand (parseInput input) []

-- Rather than add a floor and simulate again, it's much faster to find all places that CANNOT
-- be filled with sand. This is any place that has a line of 3 rocks right above it. Put rocks
-- in those places, and iterate on this until there are no new rocks.
-- I'm lucky the input doesn't have any shelves along the diagonal.
part2 input =
  let cave = addFloor $ parseInput input
      yMax = (maximum $ map snd cave) - 1  -- Lowest possible sand is right above the floor
      possibleSand = sandRange yMax
      noSand = findUnreachable cave 
  in length $ filter (`notElem` noSand) possibleSand

findUnreachable :: [Coord] -> [Coord]
findUnreachable cs =
  case newFound of
    []    -> cs
    newCs -> findUnreachable (cs ++ newCs)
  where
    bottom = (maximum $ map snd cs)
    toCheck = filter (`notElem` cs) [ (x,y) | y <- [0..bottom], x <- [500-y..500+y]]
    hasCeiling c@(x,y) = let roof = map (,y-1) [x-1..x+1]
      in all (`elem` cs) roof
    newFound = filter hasCeiling toCheck

main input = do
  solve part1 input
  solve part2 input