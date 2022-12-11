module Day10 where
import AOC (solve)
import Data.List (isInfixOf, intercalate)
import Data.List.Split (chunksOf)

type Cycles = [Int]

newRegister :: Cycles
newRegister = [1]

readProgram :: Cycles -> [String] -> Cycles
readProgram history [] = history
readProgram history (inst:insts) =
  let current = last history
      added = case words inst of
        ["noop"]       -> [current]
        ("addx":num:_) -> [current, current + (read num::Int)]
  in readProgram (history ++ added) insts

haystack `contains` needle = [needle] `isInfixOf` haystack

part1 input = do
  let registerHistory = readProgram newRegister $ lines input
  let significant = [i*x | (i,x) <- zip [1..] registerHistory, (i+20) `mod` 40 == 0]
  sum $ take 6 significant

part2 input = do
  let registerHistory = readProgram newRegister $ lines input
  let spriteHistory = map (\ x -> [x-1..x+1]) registerHistory
  let crt = [ if sprite `contains` (pixel `mod` 40) then '#'
              else                            ' '
              | (pixel, sprite) <- zip [0..] spriteHistory ]
  intercalate "\n" $ chunksOf 40 crt


main input = do
  solve part1 input
  solve part2 input