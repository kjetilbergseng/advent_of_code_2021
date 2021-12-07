module Day7 where

import Data.Sort
import UtilityFunctions

median input = sort input !! ceiling (fromIntegral (length input) / 2.0)

shiftedAverage i input = round (fromIntegral (sum input) / fromIntegral (length input)) + i

triangularNumber i = sum [1 .. i]

day7 = do
  putStrLn "day7"
  contents <- readFile "../input/day7.txt"
  let input = map readInt (split ',' contents)
  let day6a = sum $ map (abs . ((-) . median $ input)) input
  let day6b = minimum [sum $ map (triangularNumber . (abs . ((-) . shiftedAverage x $ input))) input | x <- [-2 .. 2]]

  print day6a
  print day6b