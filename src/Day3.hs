module Day3 where

import Control.Applicative
import Data.Bits (xor)

transpose :: [[a]] -> [[a]]
transpose = getZipList . traverse ZipList

mostRpeated li
  | 2 * length (filter (== '1') li) < length li = 0
  | otherwise = 1

gamma li = map mostRpeated (transpose li)

epsilon :: [Int] -> [Int]
epsilon = map (xor 1)

fn a b = 2 ^ a * b

toDecimal li = sum $ zipWith fn [0 .. length li] (reverse li)

day3 = do
  putStrLn "day3"
  contents <- readFile "../input/day3.txt"
  let input = lines contents
  let g = toDecimal . gamma $ input
  let e = toDecimal . epsilon . gamma $ input
  print (g * e)
