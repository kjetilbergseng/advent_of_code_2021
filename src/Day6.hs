module Day6 where
import UtilityFunctions

countFish days list
    | days==0 = list
    | otherwise = countFish (days-1) (transformElement 6 (+head list) (rotate 1 list))
numbersInBin n = length . filter (==n) 
createBins n input = [ numbersInBin x input | x <- [0..n]]
day6 = do
  putStrLn "day6"
  contents <- readFile "../input/day6.txt"
  let input = map readInteger (split ',' contents)
  let bins=createBins 8 input

  print . sum $ countFish 80 bins
  print . sum $ countFish 256 bins