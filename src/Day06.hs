module Day06 where
import Data.List

markerLength = 4

checkAt :: Eq a => [a] -> Int -> Bool
checkAt input n = let chars = drop (n-4) $ take n input
  in length (nub chars) == markerLength

part1 input = head $ filter (checkAt input) [1..]