module Day20 where
import AOC (solve)
import Data.List (elemIndex)
import Data.Maybe (fromJust)

-- Pairs of original index and value, so we can find
-- specific entries no matter how the file gets shuffled
type File = [(Int, Int)]

decrypt :: File -> File
decrypt f = map (\ (a,b) -> (a, b*811589153) ) f

parseInput :: String -> File
parseInput input = zip [0..] $ map read $ lines input

find :: Int -> File -> Int
find origIndex xs = head [cur | (cur, (orig, _)) <- zip [0..] xs, orig==origIndex]

mix :: File -> File
mix f = mixInner f [0..length f - 1]

mixInner :: File -> [Int] -> File
mixInner xs [] = xs
mixInner xs (i:is) = do
  let curPos = find i xs
  let delta = snd $ xs !! curPos
  let newPos = mod (curPos + delta) $ (length xs - 1)
  let removed = take curPos xs ++ drop (curPos+1) xs
  let newXs = take newPos removed ++ [xs !! curPos] ++ drop newPos removed
  mixInner newXs is

part1 input = do
  let file = parseInput input
  let mixed = map snd $ mix file
  let zero = fromJust $ elemIndex 0 mixed
  sum $ map ((mixed!!) . (`mod` (length file)) . (+zero)) [1000,2000,3000]

part2 input = do
  let file = decrypt $ parseInput input
  let mixed = (map (map snd) $ iterate mix file) !! 10
  let zero = fromJust $ elemIndex 0 mixed
  sum $ map ((mixed!!) . (`mod` (length file)) . (+zero)) [1000,2000,3000]
  
main input = do
  solve part1 input
  solve part2 input