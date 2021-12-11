module Day8 where

import Control.Monad (join)
import Data.List
import Data.Sort
import UtilityFunctions

zero li = join [x | x <- li, length x == 6, length (x \\ five li) == 2]

one li = join [x | x <- li, length x == 2]

two li = join [x | x <- li, length x == 5, length (x \\ four li) == 3]

three li = join [x | x <- li, length x == 5, length (x \\ one li) == 3]

four li = join [x | x <- li, length x == 4]

five li = join [x | x <- li, length x == 5, x \\ two li /= "", x \\ three li /= ""]

six li = join [x | x <- li, length x == 6, length (x \\ seven li) == 4]

seven li = join [x | x <- li, length x == 3]

eight li = join [x | x <- li, length x == 7]

nine li = join [x | x <- li, length x == 6, length (x \\ three li) == 1]

functionByNumber num = [zero, one, two, three, four, five, six, seven, eight, nine] !! num

checkNumber li numString num
  | sort (functionByNumber num li) == sort numString = Just num
  | otherwise = Nothing

toDigit signalPattern str = head [x | x <- [0 .. 9], checkNumber signalPattern str x == Just x]

toNumber signalPattern output = toDecimal $ map (toDigit signalPattern) output

toDecimal li = sum $ zipWith (\x y -> 10 ^ x * y) [0 .. length li] (reverse li)

day8a outputs = length $ map length (join outputs) `intersect` [2,3,4,7]

day8b signalPatterns outputs = sum $ zipWith toNumber signalPatterns outputs

day8 = do
  putStrLn "day8"
  contents <- readFile "../input/day8.txt"

  let signalPatterns = map (words . takeWhile (/= '|')) (lines contents)
  let outputs = map (words . filter (/= '|') . dropWhile (/= '|')) (lines contents)

  print $ day8a outputs
  print $ day8b signalPatterns outputs
