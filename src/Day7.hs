module Day7 where

import Data.Sort
import UtilityFunctions

median input = sort input !! div (length input) 2

shiftedAverage i input = i + div (sum input) (length input)

triangularNumber i = sum [1 .. i]

distanceFromMedian input = map (abs . ((-) . median $ input)) input

triangularDistance i = map (triangularNumber . abs . (i -))

day7 = do
  putStrLn "day7"
  contents <- readFile "../input/day7.txt"
  let input = map readInt (split ',' contents)
  let day6a = sum . distanceFromMedian $ input
  let day6b = minimum [sum $ triangularDistance (shiftedAverage x input) input | x <- [-2 .. 2]]

  print day6a
  print day6b