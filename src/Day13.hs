module Day13 where

import qualified Data.Bifunctor
import Data.List (nub)
import Data.Matrix (setElem, transpose, zero)
import UtilityFunctions (readInt, split, toPair)

foldElementY pos (x, y)
  | y < pos = (x, y)
  | otherwise = (x, abs (2 * pos - y))

foldElementX pos (x, y)
  | x < pos = (x, y)
  | otherwise = (abs (2 * pos - x), y)

foldAt li (direction, pos)
  | direction == "x" = nub $ map (foldElementX pos) (filter (\(x, y) -> y /= pos) li)
  | otherwise = nub $ map (foldElementY pos) (filter (\(x, y) -> y /= pos) li)

reduceMatrix coords m = transpose $ foldl (\m (x, y) -> setElem 1 (x + 1, y + 1) m) m coords

foldAll = foldl (\li (x, y) -> foldAt li (x, y))

day13a input fold = length $ foldAt input fold

day13b input folds = reduceMatrix coords mat
  where
    coords = foldAll input folds
    mat = zero (1 + maximum (map fst coords)) (1 + maximum (map snd coords))

day13 = do
  putStrLn "day13"
  contents <- readFile "../input/day13.txt"
  let input = map (toPair . map readInt) (takeWhile (/= []) . map (split ',') $ lines contents)
  let folds =
        map
          ( Data.Bifunctor.second readInt
              . toPair
              . split '='
              . dropWhile (\x -> x /= 'x' && x /= 'y')
          )
          (tail . dropWhile (/= []) $ lines contents)

  print $ day13a input (head folds)
  print $ day13b input folds