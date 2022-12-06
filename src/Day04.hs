module Day04 (part1, part2) where
import Data.List.Split (splitOn)

type Range = (Int, Int)
lower = fst
upper = snd

parseInput input = map parseLine $ lines input

parseLine :: String -> (Range, Range)
parseLine s = let ranges = splitOn "," s
  in (parseRange $ head ranges, parseRange $ last ranges)

parseRange :: String -> Range
parseRange s = let nums = map (\ x -> read x :: Int) $ splitOn "-" s
  in (head nums, last nums)

fullyContains :: (Ord a) => (a, a) -> (a, a) -> Bool
fullyContains (aMin, aMax) (bMin, bMax) = (bMin >= aMin) && (bMax <= aMax)

checkPair :: (Range, Range) -> Bool
checkPair (a, b) = a `fullyContains` b || b `fullyContains` a

overlaps :: (Ord a) => ((a, a), (a, a)) -> Bool
overlaps ((aMin, aMax), (bMin, bMax)) = not ( (aMin > bMax) || (bMin > aMax) )

part1 input = length $ filter checkPair $ parseInput input
part2 input = length $ filter overlaps $ parseInput input