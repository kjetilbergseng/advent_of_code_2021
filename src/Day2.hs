module Day2 where

import Control.Monad (join)
import Data.Function ((&))
import UtilityFunctions (mapReduce)

positionChange (str, int)
  | str == "forward" = (read int, 0)
  | str == "up" = (0, - read int)
  | str == "down" = (0, read int)
  | otherwise = (0, 0)

pairSum (a, b) (c, d) = (a + c, b + d)

aim (a, b, c) (d, e) = (a + d, b + d * c, c + e)

tripleToPair (a, b, _) = (a, b)

day2a li = uncurry (*) (mapReduce positionChange pairSum (0, 0) li)

day2b li =
  li & 
  mapReduce positionChange aim (0, 0, 0) & 
  tripleToPair & 
  uncurry (*)

day2 = do
  putStrLn "day2"
  contents <- readFile "../input/day2.txt"
  let input = join . map lex $ lines contents
  print $ day2a input
  print $ day2b input