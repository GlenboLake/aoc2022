module Day03 (part1, part2) where
import Data.List (elemIndex, nub)

priorities = " " ++ ['a'..'z'] ++ ['A'..'Z']

getPriority :: Char -> Int
getPriority item = do
  let value = elemIndex item priorities
  case value of Just a  -> a
                Nothing -> 0

pairToList (x,y) = [x,y]

cutInHalf :: [a] -> ([a], [a])
cutInHalf stuff = splitAt (length stuff `div` 2) stuff

common :: (Eq a) => [[a]] -> [a]
common (x:xs)
  | length xs == 0 = x
  | otherwise      = nub $ filter (`elem` x) $ common xs

groupElves :: [a] -> [[a]]
groupElves [] = []
groupElves lines = [take 3 lines] ++ (groupElves (drop 3 lines))

chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk num items =
  let (group, rest) = splitAt num items
  in group : chunk num rest

part1 :: String -> Int
part1 input = sum $ map checkRucksack $ lines input
  where checkRucksack rucksack = getPriority $ head $ common $ pairToList $ cutInHalf rucksack

part2 :: String -> Int
part2 input = sum $ map checkGroups $ chunk 3 $ lines input
  where checkGroups group = getPriority $ head $ common group