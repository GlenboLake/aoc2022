module Day07 where
import AOC (solve)
import Data.List (intercalate, partition)
import Data.List.Split (splitOn)

type CommandText = [String]
type Output = [String]
data Command = Ls Output | Cd FilePath
  deriving (Show, Eq)

a `startsWith` sub = (take (length sub) a) == sub

parseInput :: String -> [Command]
parseInput input = map parseCommand $ parseLines $ lines input

parseLines :: [String] -> [CommandText]
parseLines [] = []
parseLines (command:rest) =
  let (output, otherCommands) = break (`startsWith` "$") rest
  in [(command:output)] ++ parseLines otherCommands

parseCommand :: CommandText -> Command
parseCommand (c:output) = case words c of
  [_, "cd", a] -> Cd a
  [_, "ls"]    -> Ls output

root :: FSObj
root = ("/", Dir)

findAllFiles commands = checkShell "" [root] commands

checkShell :: FilePath -> [FSObj] -> [Command] -> [FSObj]
checkShell _ files [] = files
checkShell cwd files (c:cs) = case c of
  Cd a      -> let newCwd = cd cwd a
               in checkShell newCwd files cs
  Ls output -> let newFiles = ls cwd output
               in checkShell cwd (files++newFiles) cs

-- Path management
-- Get parent dir
parent path = intercalate "/" $ reverse $ tail $ reverse $ splitOn "/" path

joinPath parent' child = addTrailingPathSeparator parent' ++ child
-- Ensure trailing path separator
addTrailingPathSeparator "" = "/"
addTrailingPathSeparator path =
  if last path == '/' then path else path ++ "/"

-- Handle cd
cd :: FilePath -> FilePath -> FilePath
cd cwd dir = case dir of
  "/"  -> "/"
  ".." -> parent cwd
  d    -> cwd `joinPath` dir

-- Handle ls
ls :: FilePath -> [FilePath] -> [FSObj]
ls _ [] = []
ls cwd (o:os) = do
  let (size:name:_) = words o
  let fullPath = joinPath cwd name
  let fileType = if size == "dir" then Dir
                 else File (read size :: Int)
  ((fullPath, fileType)::FSObj) : (ls cwd os)

data FileType = Dir | File Int deriving (Show, Eq)
type FSObj = (FilePath, FileType)

sizeOfDir :: [FSObj] -> FilePath -> Int
sizeOfDir [] _ = 0
sizeOfDir ((name, File size):fs) prefix =
  (if name `startsWith` prefix then size else 0) + (sizeOfDir fs prefix)

getFolderSizes :: [FSObj] -> [(FilePath, Int)]
getFolderSizes objs =
  let (dirs, files) = partition (\ (_,t) -> t==Dir) objs
      dirs' = map fst dirs
  in zip dirs' (map (sizeOfDir files) $ map fst dirs)

part1 :: String -> Int
part1 input = let sizes = getFolderSizes $ findAllFiles $ parseInput input
  in sum $ filter (<=100000) $ map snd sizes

part2 :: String -> Int
part2 input = do
   let sizes = map snd $ getFolderSizes $ findAllFiles $ parseInput input
   let toFree = maximum sizes - 40_000_000
   minimum $ filter (>toFree) sizes

main input = do
  putStr "Part 1: "
  solve part1 input
  putStr "Part 2: "
  solve part2 input