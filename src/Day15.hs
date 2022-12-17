module Day15 where
import AOC (solve)
import Data.List (nub, sort)
import System.IO.Unsafe (unsafePerformIO)

sample = unsafePerformIO . readFile $ "inputs/sample15.txt"

type Coord = (Int, Int)

-- Range and associated functions
data Range = Range { left::Int, right::Int } | Nil deriving (Show, Eq, Ord)

sizeOf :: Range -> Int
sizeOf Range { left=l, right=r } = r-l+1

contains :: Range -> Int -> Bool
contains (Range l r) i = i >= l && i <= r

overlaps :: Range -> Range -> Bool
a `overlaps` b = (max (left a) (left b)) - (min (right a) (right b)) <= 1

clamp :: Range -> Range -> Range
clamp (r@Range {left=rl, right=rr}) (c@Range {left=cl, right=cr})
  | r `overlaps` c = Range (max rl cl) (min rr cr)
  | otherwise      = Nil

-- combine ranges together. This function assumes they
-- are already sorted by the min bound and Nil has been removed
combineTwoRanges :: Range -> Range -> Range
combineTwoRanges a b = Range {
  left = min (left a) (left b),
  right = max (right a) (right b)
}

combine :: [Range] -> [Range]
combine [] = []
combine [a] = [a]
combine [a,b]
  | a `overlaps` b = [combineTwoRanges a b]
  | otherwise      = [a, b]
combine (a@Range { left=la, right=ra }:b@Range { left=lb, right=rb }:rs)
  | a `overlaps` b = combine (combineTwoRanges a b:combine rs)
  | otherwise      = (a:combine (b:rs))


manhattan :: Coord -> Coord -> Int
manhattan (x1,y1) (x2,y2) = abs (x1-x2) + abs (y1-y2)

data Sensor = Sensor { loc :: Coord
                     , beacon :: Coord
                     } deriving (Show)

isNumeric c = c `elem` "-1234567890"

parseInput :: String -> [Sensor]
parseInput input = map parseSensor $ lines input
  where parseSensor line = let (a:b:c:d:_) = map (read :: String -> Int) $ filter (/="") $ map (filter isNumeric) $ words line
                           in Sensor { loc = (a, b), beacon = (c, d) }

checkSensor :: Sensor -> Int -> Range
checkSensor (Sensor { loc = l@(lx, ly), beacon = b@(bx, by) }) y
  | width <= 0 = Nil
  | otherwise  = Range (lx-width) (lx+width)
  where m = manhattan l (bx,by)
        width = m - abs (y-ly)

findBeaconsAndRanges ss bs y = do
  let beaconsOnRow = filter (==y) $ map snd bs
  let ranges = combine $ sort $ filter (/= Nil) $ map (flip checkSensor y) ss
  let beaconsInRanges = nub [ b | b <- beaconsOnRow, r <- ranges, r `contains` b ]
  (ranges, beaconsInRanges)



findHole :: [Sensor] -> Range -> [Int] -> Coord
findHole ss limit (y:ys) = case length rs of
  -- Given the problem constraints,
  1 -> findHole ss limit ys
  2 -> ((right $ head rs) + 1, y)
  -- Given the problem constraints,
  where rs = map (clamp limit) $ fst $ findBeaconsAndRanges ss [] y




part1 n input = do
  let sensors = parseInput input
  let beacons = nub $ map beacon sensors
  let (rs, bs) = findBeaconsAndRanges sensors beacons n
  (sum $ map sizeOf rs) - (length bs)

part2 n input = do
  let cap = 2*n
  let bounds = Range 0 $ 2*n
  let sensors = parseInput input
  let (x,y) = findHole sensors bounds [0..cap]
  x * 4_000_000 + y

main input y = do
  solve (part1 y) input
  solve (part2 y) input