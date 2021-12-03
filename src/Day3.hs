module Day3 where

import Control.Applicative
import Data.Bits (xor)
import GHC.Generics (Generic (to))

transpose :: [[a]] -> [[a]]
transpose = getZipList . traverse ZipList

mostRpeated li
  | 2 * length (filter (== '0') li) > length li = 0
  | otherwise = 1

mostRpeatedChar li
  | 2 * length (filter (== '0') li) > length li = '0'
  | otherwise = '1'

gamma li = map mostRpeated (transpose li)

epsilon :: [Int] -> [Int]
epsilon = map (xor 1)

toDecimal li = sum $ zipWith (\x y -> 2 ^ x * y) [0 .. length li] (reverse li)

compareEqual i l1 l2 = l1 !! i == l2 !! i

compareNotEqual i l1 l2 = not (compareEqual i l1 l2)

mapMostRepeated li = map mostRpeatedChar (transpose li)

filterMostrepeated iter compareFn li = filter (compareFn iter (mapMostRepeated li)) li

rating compareFn li = map charBitToInt (ratingChar 0 compareFn li)

ratingChar i compareFn li
  | length li == 1 = head li
  | i == length (head li) = head li
  | otherwise = ratingChar (i + 1) compareFn (filterMostrepeated i compareFn li)

charBitToInt :: Char -> Int
charBitToInt i
  | i == '0' = 0
  | otherwise = 1

day3 = do
  putStrLn "day3"
  contents <- readFile "../input/day3.txt"
  let input = lines contents
  let gammaDecimal = toDecimal . gamma $ input
  let epsilonDecimal = toDecimal . epsilon . gamma $ input
  let oxygen = toDecimal (rating compareEqual input)
  let co2 = toDecimal (rating compareNotEqual input)
  print (gammaDecimal * epsilonDecimal)
  print (oxygen * co2)