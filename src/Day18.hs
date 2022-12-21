module Day18 where
import AOC (solve)
import Data.List (nub, sort)

type Point = (Int, Int, Int)

parseInput :: String -> [Point]
parseInput input = map (\ line -> read ("("++line++")")::Point) $ lines input

adjacent :: Point -> [Point]
adjacent (x,y,z) = [ (x+1,y,z)
                   , (x-1,y,z)
                   , (x,y+1,z)
                   , (x,y-1,z)
                   , (x,y,z+1)
                   , (x,y,z-1)
                   ]

adjacentWithDiag :: Point -> [Point]
adjacentWithDiag (x,y,z) = [ (a,b,c) | a <- [x-1..x+1], b <- [y-1..y+1], c <- [z-1..z+1], (a,b,c) /= (x,y,z) ]

exposedSides :: [Point] -> Point -> Int
exposedSides others p = 6 - (length $ filter (`elem` others) $ adjacent p)

dropletSurface :: [Point] -> Int
dropletSurface ps = sum $ map (exposedSides ps) ps

getAllAir :: [Point] -> [Point]
getAllAir ps = filter (`notElem` ps) $ nub $ concatMap adjacentWithDiag ps

findConnected :: [Point] -> [Point] -> [Point]
findConnected ps [] = ps
findConnected blob pool = case filter (`elem` pool) $ concatMap adjacent blob of
  [] -> blob
  ps -> findConnected (nub $ blob ++ ps) (filter (`notElem` ps) pool)
  

part1 input = dropletSurface $ parseInput input

part2 input = do
  let ps = parseInput input
  let air = getAllAir ps
  let exteriorAir = findConnected [minimum air] air
  length $ filter (`elem` exteriorAir) $ concatMap adjacent ps

main input = do
  solve part1 input
  solve part2 input