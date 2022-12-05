module Day05 where
import Data.List.Split (splitOn)

type Layout = [String]
type Step = (Int, Int, Int)

parseInput :: String -> (Layout, [Step])
parseInput input =
  let sections = splitOn "\n\n" input
      -- We know there will only be 2 sections
      layout = lines $ head sections
      procedure = lines $ last sections
  in (parseLayout layout, parseProcedure procedure)

parseLayout layout = do
  let numColumns = length $ filter (/=' ') $ last $ layout
  let stacks = reverse $ tail $ reverse $ layout
  let stackIndices = map (\x -> x*4-3) [1..numColumns]
  let extract column = filter (/=' ') $ map (!! column) stacks
  map extract stackIndices

parseStep line = do
  let w = words line
  let nums = map (\ i -> read (w!!i)::Int) [1,3,5]
  let adjusted = [head nums] ++ (map (\ x -> x-1) $ tail nums)
  (adjusted!!0, adjusted!!1, adjusted!!2)

parseProcedure procedure = map parseStep procedure

move :: Step -> Layout -> Layout
move (0,_,_) layout = layout
move (n,from,to) layout = do
  let movedCrate = head $ layout!!from
  let newLayout = [if i==from then tail stack else if i==to then movedCrate:stack else stack | (i, stack) <- zip [0..] layout]
  move (n-1,from,to) newLayout

moveSeveral :: Step -> Layout -> Layout
moveSeveral (n, from, to) layout = do
  let movedCrates = take n $ layout!!from
  [if i==from then drop n stack else if i==to then movedCrates++stack else stack | (i, stack) <- zip [0..] layout]

applyProcedure :: (Step -> Layout -> Layout) -> Layout -> [Step] -> Layout
applyProcedure _ layout [] = layout
applyProcedure moveFunc layout (step:steps) = applyProcedure moveFunc (moveFunc step layout) steps

solve input moveFunc = let (layout, procedure) = parseInput input in map head $ applyProcedure moveFunc layout procedure

part1 input = solve input move
part2 input = solve input moveSeveral