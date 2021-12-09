module Day9 where

import Data.Bits (xor)
import Data.List (transpose)
import UtilityFunctions

checkLeft li = zipWith (<) li (tail li) <> [True]

checkRight li = reverse $ checkLeft (reverse li)

find1DLowPoint li = zipWith (&&) (checkLeft li) (checkRight li)

--findLowPoints li = map (zipWith (&&)) (map (map find1DLowPoint li) (map find1DLowPoint (transpose li))

rows = map find1DLowPoint

cols li = transpose $ map find1DLowPoint (transpose li)

findLowPoints li = zipWith (zipWith (&&)) (rows li) (cols li)

localRiskLevel x y
  | y = read x + 1
  | otherwise = 0

findRiskLevel li = zipWith (zipWith localRiskLevel) li (findLowPoints li)

day9a input = sum $ map sum (findRiskLevel input)

day9 = do
  putStrLn "day9"
  contents <- readFile "../input/day9.txt"
  let input = map splitEachChar (lines contents)
  print $ day9a input