module AOC (solve) where

escapingPrint :: Show b => b -> IO ()
escapingPrint thing = do
  let s = show thing
  let printable = if head s == '"' then read s::String
                                   else s
  putStrLn printable

solve :: Show b => (String -> b) -> String -> IO ()
solve part fileName = do
  input <- readFile fileName
  escapingPrint $ part input