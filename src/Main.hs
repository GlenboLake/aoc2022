module Main where
import AOC (solve)
import System.Environment (getArgs)
import qualified Day01 (part1, part2)
import qualified Day02 (part1, part2)
import qualified Day03 (part1, part2)
import qualified Day04 (part1, part2)
import qualified Day05 (part1, part2)
import qualified Day06 (part1, part2)
import qualified Day07 (part1, part2)
import qualified Day08 (part1, part2)
import qualified Day09 (part1, part2)
import qualified Day10 (part1, part2)
import qualified Day11 (part1, part2)
import qualified Day12 (part1, part2)
import qualified Day13 (part1, part2)
import qualified Day14 (part1, part2)
import qualified Day15 (part1, part2)
import qualified Day16 (part1)
import qualified Day18 (part1, part2)
import qualified Day20 (part1, part2)

type DayFunc = (String -> String)

unsolved _ = "unsolved :("

getDay :: Int -> (DayFunc, DayFunc)
getDay 1 = (show . Day01.part1, show . Day01.part2)
getDay 2 = (show . Day02.part1, show . Day02.part2)
getDay 3 = (show . Day03.part1, show . Day03.part2)
getDay 4 = (show . Day04.part1, show . Day04.part2)
getDay 5 = (Day05.part1, Day05.part2)
getDay 6 = (show . Day06.part1, show . Day06.part2)
getDay 7 = (show . Day07.part1, show . Day07.part2)
getDay 8 = (show . Day08.part1, show . Day08.part2)
getDay 9 = (show . Day09.part1, show . Day09.part2)
getDay 10 = (show . Day10.part1, ("\n" ++) . Day10.part2)
getDay 11 = (show . Day11.part1, show . Day11.part2)
getDay 12 = (show . Day12.part1, show . Day12.part2)
getDay 13 = (show . Day13.part1, show . Day13.part2)
getDay 14 = (show . Day14.part1, show . Day14.part2)
getDay 15 = (show . (Day15.part1 2_000_000), show . (Day15.part2 2_000_000))
getDay 16 = (show . Day16.part1, unsolved)
getDay 17 = (unsolved, unsolved)
getDay 18 = (show . Day18.part1, show . Day18.part2)
getDay 19 = (unsolved, unsolved)
getDay 20 = (show . Day20.part1, show . Day20.part2)

getInput :: Int -> FilePath
getInput day = "inputs/day" ++ padded ++ ".txt"
  where
    padded :: String
    padded
      | day < 10  = '0':(show day)
      | otherwise = show day

runDay :: Int -> IO ()
runDay day = do
  let (p1, p2) = getDay day
  let file = getInput day
  putStr "Part 1: "
  solve p1 file
  putStr "Part 2: "
  solve p2 file

main :: IO ()
main = do
  args <- getArgs
  let day = read $ head args
  runDay day
