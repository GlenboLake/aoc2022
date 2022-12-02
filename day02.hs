data Throw = Rock | Paper | Scissors deriving (Enum, Show)
type Matchup = (Throw, Throw)

parseInput :: String -> [Matchup]
parseInput input = map parseLine (lines input)

asThrow :: Char -> Throw
asThrow 'A' = Rock
asThrow 'B' = Paper
asThrow 'C' = Scissors
asThrow 'X' = Rock
asThrow 'Y' = Paper
asThrow 'Z' = Scissors

parseLine :: String -> Matchup
parseLine line = (asThrow (head line), asThrow (last line))

scoreThrow :: Throw -> Int
scoreThrow Rock = 1
scoreThrow Paper = 2
scoreThrow Scissors = 3

scoreWinner :: Matchup -> Int
scoreWinner (Rock, Scissors) = 0
scoreWinner (Scissors, Paper) = 0
scoreWinner (Paper, Rock) = 0
scoreWinner (Rock, Rock) = 3
scoreWinner (Paper, Paper) = 3
scoreWinner (Scissors, Scissors) = 3
scoreWinner (Rock, Paper) = 6
scoreWinner (Paper, Scissors) = 6
scoreWinner (Scissors, Rock) = 6

scoreMatchup :: Matchup -> Int
scoreMatchup (a,b) = scoreThrow b + scoreWinner (a,b)

part1 input = sum [scoreMatchup m | m <- parseInput input]

