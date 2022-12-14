module Day13 where
import AOC (solve)
import Data.List (elemIndices, sort)
import Data.List.Split (splitOn)
import System.IO.Unsafe (unsafePerformIO)

data Packet = I Int | P [Packet] deriving (Show, Eq, Read)

sample = unsafePerformIO . readFile $ "inputs/sample13.txt"

instance Ord Packet where
  compare (I a) (I b) = compare a b
  compare (P as) (P bs) = compare as bs
  compare (I a) (P bs) = compare (P [I a]) (P bs)
  compare (P as) (I b) = compare (P as) (P [I b])

isNumeric c = c `elem` "-1234567890"

readPacket :: String -> Packet
readPacket "" = P []
readPacket s = read $ formatAsPacket s
  where
    formatAsPacket "" = ""
    formatAsPacket str@(c:cs)
      | c `elem` ", ]" = c : formatAsPacket cs
      | c == '['       = "P [" ++ formatAsPacket cs
      | otherwise      = "I " ++ (takeWhile isNumeric str) ++ formatAsPacket (dropWhile isNumeric str)


--checkPair

parseInput input = map readPacket $ filter (/="") $ lines input

checkPairs [] = []
checkPairs (a:b:ps) = (compare a b /= GT) : checkPairs ps

part1 input = sum $ map fst $ filter snd $ zip [1..] $ checkPairs $ parseInput input

part2 input = let anchors = map readPacket ["[[2]]", "[[6]]"]
                  packets = sort $ parseInput input ++ anchors
              in product $ concat $ map (\ x -> map succ $ elemIndices x packets) anchors

main input = do
  solve part1 input
  solve part2 input