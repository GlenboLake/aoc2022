module Day12 where
import qualified AOC (solve)
import qualified Data.Map as M
import Data.Map((!))

type Coord = (Int, Int)

heights = M.fromList $ (('S',0):('E',25):zip ['a'..'z'] [0..])

parseInput :: String -> M.Map Coord Char
parseInput input = M.fromList [((r,c),v) | (r,line) <- zip [0..] $ lines input, (c,v) <- zip [0..] line]

dests grid pos@(r,c) =
  let options = filter (flip M.member grid) [(r+1,c), (r-1,c), (r,c+1), (r,c-1)]
      maxHeight = heights ! (grid ! pos) + 1
  in filter (\o -> heights ! (grid ! o) <= maxHeight) options

solve :: M.Map Coord Char -> Coord -> Coord -> M.Map Coord Int
solve grid start end = innerSolve grid [start] $ M.fromList [(start, 0)]

innerSolve _ [] seen = seen
innerSolve grid (c:cs) seen = do
  let newDist = seen ! c + 1
  let ds = filter (\ d -> M.findWithDefault (maxBound::Int) d seen > newDist) $ dests grid c
  let newQ = cs ++ (filter (`notElem` cs) ds)
  let newSeen = M.union (M.fromList $ map ((,newDist)) ds) seen
  innerSolve grid newQ newSeen


part1 input = solve grid start end ! end
  where grid = parseInput input
        start = head $ M.keys $ M.filter (=='S') grid
        end = head $ M.keys $ M.filter (=='E') grid

part2 input = minimum $ map (\ s -> M.findWithDefault maxBound end (solve grid s end) ) starts
  where grid = parseInput input
        starts = M.keys $ M.filter (`elem` "Sa") grid
        end = head $ M.keys $ M.filter (=='E') grid
        

main input = do
  AOC.solve part1 input
  AOC.solve part2 input