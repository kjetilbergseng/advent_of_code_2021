module Day10 where

import Data.List ( intersect )
import Data.Sort ()
import Day7 (median)

isLeft c = length ([c] `intersect` "[<({") == 1

matches l r
  | l == '(' && r == ')' = True
  | l == '[' && r == ']' = True
  | l == '{' && r == '}' = True
  | l == '<' && r == '>' = True
  | otherwise = False

scorePartA c
  | c == ')' = 3
  | c == ']' = 57
  | c == '}' = 1197
  | c == '>' = 25137
  | otherwise = 0

scorePartB c
  | c == '(' = 1
  | c == '[' = 2
  | c == '{' = 3
  | c == '<' = 4
  | otherwise = 0

findIllegal lefts li
  | null li = 0
  | isLeft (head li) = findIllegal (head li:lefts) (tail li)
  | null lefts = 0
  | matches (head lefts) (head li) = findIllegal (tail lefts) (tail li)
  | otherwise = scorePartA (head li)

findMissingRights lefts li
  | null li = foldl (\lhs rhs -> lhs*5 + scorePartB rhs) 0 lefts
  | isLeft (head li) = findMissingRights (head li:lefts) (tail li)
  | null lefts = 0
  | matches (head lefts) (head li) = findMissingRights (tail lefts) (tail li)
  | otherwise = 0

day10a input= sum $ map (findIllegal []) input

day10b input=  median . filter (/=0) $ map (findMissingRights []) input

day10 = do
  putStrLn "day10"
  contents <- readFile "../input/day10.txt"
  let input = lines contents
  print $ day10a input
  print $ day10b input
