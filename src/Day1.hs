module Day1 where

--day1
f a b
  | b > a = 1
  | otherwise = 0

sum3 a b c = a + b + c

day1a li = sum . zipWith f li $ tail li

day1b li = zipWith3 sum3 li (tail li) (drop 2 li)

toInt x = read x :: Int

day1 = do
  putStrLn "day1"
  contents <- readFile "../input/day1.txt"
  print . day1a $ map toInt (lines contents)
  print . day1a . day1b $ map toInt (lines contents)