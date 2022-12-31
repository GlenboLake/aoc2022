module Day16 where
import qualified AOC (solve)
import Data.List (nub, sort)
import qualified Data.Map as M
import Data.Map (Map, fromList, (!))
import qualified Data.Set as S

data Valve = Valve { label :: String
                   , flowRate :: Int
                   , connections :: [String]
} deriving (Show)

type Graph = Map String Valve

parseInput :: String -> Graph
parseInput input = fromList $ map (\v -> (label v, v)) $ map parseValve $ lines input
  where
    parseValve line = Valve { label=name
                            , flowRate=read (drop 5 rate)
                            , connections = map (filter (/=',')) cs
                            }
      where
        (_:name:_:_:rate:_:_:_:_:cs) = words $ filter (/=';') line

type DistanceMap = Map String (Map String Int)

distanceMap :: Graph -> DistanceMap
distanceMap g = do
  let useful = map label $ filter (\ v -> flowRate v > 0 ) $ M.elems g
  let starts = "AA" : useful
  let fromValve v = (v, M.fromList $ map (bfs g v) $ filter (/= v) useful)
  M.fromList $ map fromValve starts

bfs :: Graph -> String -> String -> (String, Int)
bfs g src dest = (dest, seek [src] 0)
  where
    seek :: [String] -> Int -> Int
    seek nodes dist
      | dest `elem` conns = dist+1
      | otherwise         = seek conns (dist+1)
        where
          conns = nub $ concatMap (connections . (g!)) nodes

type State = (Int, S.Set String, String, Int) -- Time left, open valves, current location, total pressure
type StateKey = (Int, S.Set String, String)
type History = Map StateKey Int

-- Get the current flow rate of a state for a given graph
flow :: Graph -> State -> Int
flow g s@(_, vs, _, _) = sum $ map flowRate $ filter ((`elem` vs) . label) $ M.elems g

minPressure :: Graph -> State -> Int
minPressure g s@(t, _, _, p) = p + t * (flow g s)

maxFlow :: Graph -> Int
maxFlow g = sum $ map flowRate $ M.elems g

maxPressure :: Graph -> State -> Int
maxPressure g s@(t, _, _, p) = p + t * (maxFlow g)

key :: State -> StateKey
key (t, vs, l, _) = (t, vs, l)

solve :: Graph -> DistanceMap -> Int
solve g ds = seek [initState] M.empty
  where
    initState = (30, S.empty, "AA", 0)
    seek :: [State] -> History -> Int
    seek [] h = maximum $ M.elems h
    seek (s@(t, open, loc, pres):ss) h
      | null options                = seek ss updatedHistory
      | maxPressure g s < highScore = seek ss updatedHistory
      | otherwise                   = seek (ss ++ options) updatedHistory
      where
        valvesWithFlow = map label $ filter ((>0) . flowRate) $ M.elems g
        closedValves = filter (`notElem` open) $ valvesWithFlow
        moveTo :: String -> State
        moveTo dest = (t-(dist+1), S.insert dest open, dest, pres+(flow g s)*(dist+1))
          where dist = ds ! loc ! dest
        options :: [State]
        options = map moveTo closedValves
        highScore :: Int
        highScore = M.findWithDefault (-1) (key s) h
        newScores = map (minPressure g) options
        newHistoryItems = M.fromList $ zip (map key options) newScores
        updatedHistory = M.unionWith max newHistoryItems h



--    initState = (30, [], "AA", 0)
--    -- What would the final pressure be if no new valves were opened?
--    scoreState :: State -> Int
--    scoreState s@(t,os,_,p) = p + t * (pressurePerTick s)
--    -- How much pressure is release every minute for this state?
--    pressurePerTick :: State -> Int
--    pressurePerTick (_,os,_,_) = sum $ map flowRate $ M.elems $ M.filter ((`elem` os) . label) g
--    -- Run A*
--    seek :: [State] -> Map State Int -> Int
--    seek [] history = maximum $ M.elems history
--    seek (s@(t, os, l, p):ss) history
--      | null newStates = seek ss history
--      | otherwise      = seek (ss++newStates) newHistory
--      where
--        -- Destinations are open valves that are close enough to get to and open
--        dests = M.filterWithKey (\ k a -> k `notElem` os && t - a > 1) $ ds!l
--        travel dest dist = (t-(dist+1), dest:os, dest, p + (pressurePerTick s)*(dist+1))
--        newStates = M.elems $ M.mapWithKey travel dests
--        newScores = map (\ s@(_,_,_,p) -> (s,p) ) newStates
--        newHistory = M.union (M.fromList newScores) history
        

part1 input = solve g ds
  where
    g = parseInput input
    ds = distanceMap g

main input = do
  AOC.solve part1 input