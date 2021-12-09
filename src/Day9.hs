module Day9 where

import Data.List (transpose)
import UtilityFunctions
import Data.Sort
checkLeft li = zipWith (<) li (tail li) <> [True]

checkRight li = reverse $ checkLeft (reverse li)

find1DLowPoint li = zipWith (&&) (checkLeft li) (checkRight li)

rows = map find1DLowPoint

cols li = transpose $ map find1DLowPoint (transpose li)

findLowPoints li = zipWith (zipWith (&&)) (rows li) (cols li)

localRiskLevel x y
  | y = read x + 1
  | otherwise = 0

findRiskLevel li = zipWith (zipWith localRiskLevel) li (findLowPoints li)

day9a input = sum $ map sum (findRiskLevel input)

getVal arr i j
  | i<0 || j<0 || i>= length arr || j>= length (arr !! i) = True
  | otherwise = arr !! i !! j


findBasinSize i j (count, checked) 
  | getVal checked i j = (count, checked)
  | otherwise =   
    findBasinSize (i+1) j ( 
    findBasinSize (i-1) j (
    findBasinSize i (j+1) (
    findBasinSize i (j-1) (count+1, replace2d i j True checked))))

findAllBasins i j checked basins
  | i>= length checked = basins
  | getVal checked i j && j>= length (checked !! i) = findAllBasins (i+1) 0 checked basins
  | getVal checked i j =  findAllBasins i (j+1) checked basins
  | otherwise = findAllBasins i (j+1) updatedChecked (count:basins)
  where (count, updatedChecked) = findBasinSize i j (0, checked)

day9b input = head sortedBasins * sortedBasins !! 1 * sortedBasins !! 2
  where sortedBasins= reverse . sort $ findAllBasins 0 0 input []

day9 = do
  putStrLn "day9"
  contents <- readFile "../input/day9.txt"
  let input = map splitEachChar (lines contents)
  print $ day9a input
  let input2 = map (map (=="9")) input
  print $ day9b input2