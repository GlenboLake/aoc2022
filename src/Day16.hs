module Day16 where
import Data.List (nub)
import qualified Data.Map as M
import Data.Map (Map, (!))
import qualified Data.Set as S

type RoomID = String

startRoom :: RoomID
startRoom = "AA"

data Valve = Valve
  { label :: RoomID
  , flowRate :: Int
  , connections :: [RoomID]
} deriving (Show)

type Graph = Map RoomID Valve

usefulNodes :: Graph -> [RoomID]
usefulNodes g = map label $ filter ((>0) . flowRate) $ M.elems g

parseInput :: String -> Graph
parseInput input = M.fromList $ map (\v -> (label v, v)) $ map parseValve $ lines input
  where
    parseValve line = Valve { label=name
                            , flowRate=read (drop 5 rate)
                            , connections = map (filter (/=',')) cs
                            }
      where
        (_:name:_:_:rate:_:_:_:_:cs) = words $ filter (/=';') line

type DistanceMap = Map RoomID (Map RoomID Int)

bfs :: Graph -> RoomID -> RoomID -> (String, Int)
-- Start at distance of 1 to account for the minute used to open the valve
bfs g src dest = (dest, seek [src] 1)
  where
    seek :: [String] -> Int -> Int
    seek nodes dist
      | dest `elem` conns = dist+1
      | otherwise         = seek conns (dist+1)
        where
          conns = nub $ concatMap (connections . (g!)) nodes

distanceMap :: Graph -> DistanceMap
distanceMap g = do
  let useful = map label $ filter (\ v -> flowRate v > 0 ) $ M.elems g
  let starts = startRoom : useful
  let fromValve v = (v, M.fromList $ map (bfs g v) $ filter (/= v) useful)
  M.fromList $ map fromValve starts

data State = State
  { timeLeft :: Int
  , location :: RoomID
  , openValves :: S.Set RoomID
  } deriving (Show, Eq, Ord)

type History = M.Map State Int

-- State functions
emptyState :: State
emptyState = State { timeLeft = 30, location = startRoom, openValves = S.empty }

emptyHistory :: History
emptyHistory = M.fromList [(emptyState, 0)]

pressurePerTick :: State -> Graph -> Int
pressurePerTick s g = sum $ map flowRate $ filter (\v -> (label v) `elem` (openValves s)) $ M.elems g

neighbors :: State -> Int -> Graph -> DistanceMap -> [(State, Int)]
neighbors s p g dm = do
  let closedDests = M.filterWithKey (\ room dist -> room `notElem` (openValves s) ) $ dm ! location s
      nearbyDests = M.filter (< timeLeft s) closedDests
  [ (State { timeLeft=timeLeft s - ticks, openValves=S.insert room $ openValves s, location=room },
     p + ticks * (pressurePerTick s g)) | (room, ticks) <- M.toList nearbyDests ]

finalPressure :: Graph -> State -> Int -> Int
finalPressure g s cp = cp + timeLeft s * pressurePerTick s g

-- Actual solution

search :: Graph -> Int
search g = doSearch [emptyState] emptyHistory
  where
    dm = distanceMap g
    doSearch :: [State] -> History -> Int
    doSearch [] h = maximum $ M.elems $ M.mapWithKey (finalPressure g) h
    doSearch (s:ss) h = doSearch (ss ++ M.keys addedStates) (M.union addedStates h)
      where
        currentPressure = h ! s
        neighborStates = neighbors s currentPressure g dm
        addedStates = M.fromList $ filter isBetterScore neighborStates
        isBetterScore :: (State, Int) -> Bool
        isBetterScore (score, pressure) = pressure > (M.findWithDefault (-1) score h)

part1 input = search $ parseInput input