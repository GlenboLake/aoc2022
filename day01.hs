import Data.List (sort)
import Data.List.Split

getElves :: String -> [Integer]
getElves input = do
    let elves = [ [read line::Integer | line <- lines elf] | elf <- (splitOn "\n\n" input) ]
    [sum(elf) | elf <- elves]

part1 :: String -> Integer
part1 input = maximum (getElves input)

part2 :: String -> Integer
part2 input = sum (take 3 (reverse (sort (getElves input))))

