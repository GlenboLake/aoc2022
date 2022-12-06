module Day06 (part1, part2) where
import Data.List

checkAt :: Eq a => [a] -> Int -> Int -> Bool
checkAt input markerLength n = let chars = drop (n-markerLength) $ take n input
  in length (nub chars) == markerLength

solve input markerLength = head $ filter (checkAt input markerLength) [1..]

part1 input = solve input 4
part2 input = solve input 14