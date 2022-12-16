module Day15 where
import AOC (solve)
import Data.List (nub)
import System.IO.Unsafe (unsafePerformIO)

sample = unsafePerformIO . readFile $ "inputs/sample15.txt"

type Coord = (Int, Int)

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

checkSensor :: Sensor -> Int -> [Coord]
checkSensor (Sensor { loc = l@(lx, ly), beacon = b@(bx, by) }) y = do
  let m = manhattan l (bx,by)
  let width = m - abs (y-ly)
  let cs = map (,y) [lx-width..lx+width]
  filter (/=b) cs

checkRow :: [Sensor] -> Int -> [Coord]
checkRow [] _ = []
checkRow (s:ss) y = nub $ checkSensor s y ++ checkRow ss y

part1 input = length $ checkRow (parseInput input) 2_000_000

main input = do
  solve part1 input