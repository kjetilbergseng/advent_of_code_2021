module Day1 where

sum3 a b c = a + b + c
day1a li = length . filter id $ zipWith (<) li (tail li)
day1b li = zipWith3 sum3 li (tail li) (drop 2 li)

day1 = do
  putStrLn "day1"
  contents <- readFile "../input/day1.txt"
  let input = map read (lines contents)
  print . day1a $ input
  print . day1a . day1b $ input