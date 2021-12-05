module Day5 where

import qualified Data.Map as Map
import Data.Foldable (concatMap)
data Point = Point { x :: Int
                     , y :: Int
                     } deriving (Eq, Ord, Show)

slope p1 p2=fromIntegral (y p1 - y p2)/fromIntegral (x p1 - x p2)

insertLine (p1,p2) = [Point xn yn |
   xn <- [min (x p1) (x p2) .. max (x p1) (x p2)],
   yn <- [min (y p1) (y p2) .. max (y p1) (y p2)],
   slope p1 p2 == slope (Point xn yn) p2 ||
   xn == x p2 && yn == y p2 ]

parse s=
  (Point 
  (read . takeWhile (/=',') $ s) 
  (read . tail . takeWhile (/=' ') . dropWhile (/=',') $ s),
  Point 
  (read . takeWhile (/=',') . drop 4 . dropWhile (/=' ') $ s)
  (read . tail . dropWhile (/=',') . drop 4 . dropWhile (/=' ') $  s))

insertLines = concatMap insertLine
toHashMapList input = zip input (repeat 1)
toHashMap = Map.fromListWith (+)
countValueThanOne hashMap= length $ filter (>1) (Map.elems hashMap)
solve = countValueThanOne . toHashMap . toHashMapList . insertLines 

day5 = do
  putStrLn "day5"
  contents <- readFile "../input/day5.txt"
  let input = map parse (lines contents)
  let filteredInput = filter (\(p1,p2) -> x p1 == x p2 || y p1 == y p2) input
  
  print . solve $ filteredInput
  print . solve $ input