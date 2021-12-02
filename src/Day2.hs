module Day2 where

import Control.Monad (join)
import Data.Function ((&))

toInt x = read x :: Int

postionChange (str, int)
  | str == "forward" = (toInt int, 0)
  | str == "up" = (0, - toInt int)
  | str == "down" = (0, toInt int)
  | otherwise = (0, 0)

tplSum (a, b) (c, d) = (a + c, b + d)

aim (a, b, c) (d, e, _) = (a + d, b + d * c, c + e)

pairToTriple (a, b) = (a, b, 0)

tripleToPair (a, b, _) = (a, b)

mapReduce transform reduce init li = foldl reduce init (map transform li)

day2a li = uncurry (*) (mapReduce postionChange tplSum (0, 0) li)

day2b li =
  li
    & mapReduce (pairToTriple . postionChange) aim (0, 0, 0)
    & tripleToPair
    & uncurry (*)

day2 = do
  putStrLn "day2"
  contents <- readFile "../input/day2.txt"
  let input = join . map lex $ lines contents
  print $ day2a input
  print $ day2b input