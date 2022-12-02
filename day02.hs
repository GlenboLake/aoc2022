data Throw = Rock | Paper | Scissors deriving (Enum, Show, Bounded)
type Matchup = (Throw, Throw)

allThrows = [minBound..]::[Throw]

-- Logic for scoring a game

scoreThrow :: Throw -> Int
scoreThrow Rock = 1
scoreThrow Paper = 2
scoreThrow Scissors = 3

beats :: Throw -> Throw -> Bool
Rock `beats` Scissors  = True
Scissors `beats` Paper = True
Paper `beats` Rock     = True
_ `beats` _            = False

scoreWinner :: Matchup -> Int
scoreWinner (them, me)
    | me `beats` them = 6
    | them `beats` me = 0
    | otherwise       = 3

scoreMatchup :: Matchup -> Int
scoreMatchup (them, me) = scoreThrow me + scoreWinner (them, me)

-- Logic for deciding how to win/lose/draw

deduceThrow :: Throw -> Char -> Throw
deduceThrow them must
    -- must win
    | must == 'Z' = head [me | me <- allThrows, me `beats` them]
    -- must lose
    | must == 'X' = head [me | me <- allThrows, them `beats` me]
    -- must dra
    | otherwise   = them

-- Input parsing

asThrow :: Char -> Throw
asThrow 'A' = Rock
asThrow 'B' = Paper
asThrow 'C' = Scissors
asThrow 'X' = Rock
asThrow 'Y' = Paper
asThrow 'Z' = Scissors

parseInput :: (String -> Matchup) -> String -> [Matchup]
parseInput parser input = map parser (lines input)

-- Part 1: Second column is a throw
parseLineTwoThrows :: String -> Matchup
parseLineTwoThrows line = (asThrow (head line), asThrow (last line))

-- Part 2: Second column is whether to win
parseLineStrategy :: String -> Matchup
parseLineStrategy line =
    let them = asThrow (head line)
    in (them, deduceThrow them (last line))

solve strategy input = sum [scoreMatchup m | m <- parseInput strategy input]

part1 input = solve parseLineTwoThrows input
part2 input = solve parseLineStrategy input
