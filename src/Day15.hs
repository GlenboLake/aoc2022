module Day15 where
import AOC (solve)
import Data.List (nub, sort)
import System.IO.Unsafe (unsafePerformIO)

sample = unsafePerformIO . readFile $ "inputs/sample15.txt"

type Coord = (Int, Int)
data Range = Range { left::Int, right::Int } | Nil deriving (Show, Eq, Ord)

sizeOf :: Range -> Int
sizeOf Range { left=l, right=r } = r-l+1

contains :: Range -> Int -> Bool
contains (Range l r) i = i >= l && i <= r

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

-- combine ranges together. This function assumes they
-- are already sorted by the min bound and Nil has been removed
overlaps :: Range -> Range -> Bool
a `overlaps` b = (max (left a) (left b)) - (min (right a) (right b)) <= 1

combineTwoRanges :: Range -> Range -> Range
combineTwoRanges a b = Range {
  left = min (left a) (left b),
  right = max (right a) (right b)
}

combine :: [Range] -> [Range]
combine [] = []
combine [a] = [a]
combine [a,b] = [combineTwoRanges a b]
combine (a@Range { left=la, right=ra }:b@Range { left=lb, right=rb }:rs)
  | lb > ra + 1 = (a:combine (b:rs))
  | otherwise   = combine (combineTwoRanges a b:combine rs)

part1 input = do
  let y = 2_000_000
  let sensors = parseInput input
  let beacons = nub $ map beacon sensors
  let beaconsOnRow = filter (==y) $ map snd beacons
  let ranges = combine $ sort $ filter (/= Nil) $ map (flip checkSensor y) $ sensors
  let numBeaconsInRanges = length $ nub [ b | b <- beaconsOnRow, r <- ranges, r `contains` b ]
  (sum $ map sizeOf ranges) - numBeaconsInRanges

main input = do
  solve part1 input