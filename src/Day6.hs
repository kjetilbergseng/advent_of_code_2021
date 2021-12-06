module Day6 where
import UtilityFunctions
import Control.Monad (join)

countFish days list
    | days==0 = list
    | otherwise = countFish (days-1) (zipWith (+) (rotate 1 list) [0,0,0,0,0,0,head list,0,0])
numberInBin n = length . filter (==n) 
createBins n input = [ numberInBin x input | x <- [0..n]]
day6 = do
  putStrLn "day6"
  contents <- readFile "../input/day6.txt"
  let input = map readInteger (split ',' contents)
  let bins=createBins 8 input

  print . sum $ countFish 80 bins
  print . sum $ countFish 256 bins