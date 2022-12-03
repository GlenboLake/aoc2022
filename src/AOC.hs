module AOC (solve) where

solve :: Show b => (String -> b) -> String -> IO ()
solve part fileName = do
  input <- readFile fileName
  print $ part input