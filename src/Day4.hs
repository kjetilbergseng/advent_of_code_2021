{-# LANGUAGE ParallelListComp #-}

module Day4 where
import Data.List ( transpose )
import Control.Monad (join)

splitIntoBlocksOf n ls
    | n <= 0 ||null ls = []
    | otherwise = take n ls:splitIntoBlocksOf n (drop n ls)

split sep ls
    | null ls = []
    | otherwise = takeWhile (/= sep) ls:split sep (drop 1 $ dropWhile (/= sep) ls)

splitOn sep = concatMap (split sep)

mask boards numbers=[[[number `elem` numbers | number<-row] | row <-board] | board <- boards]

getBoard boards mask = filter (/=[]) [[board | row<-m, and row ] | board <- boards | m<-mask]

flattenBoard board= join $ head $ head board

bingo i boards numbers
    | board /= [] = (flattenBoard board, i)
    | otherwise = bingo (i+1) boards numbers
    where board = getBoard boards $ mask boards (take i numbers)   

day4a boards numbers
    | snd row < snd col = row 
    | otherwise = col
    where row = bingo 1 boards numbers 
          col = bingo 1 (map transpose boards) numbers

getNthNumber n numbers= read $ last (take n numbers)
getSumOfRemaining board numbers = sum $ map read $ remainingNumbers (fst board) $ take (snd board) numbers
calculateDay4Solution board numbers = getNthNumber (snd board) numbers * getSumOfRemaining board numbers

day4b boards numbers = foldl1 (getLastBoard numbers) [day4a [board] numbers | board <- boards]

remainingNumbers board numbers=[number | number <- board, number `notElem` numbers]

getLastBoard numbers board1 board2 
    | snd board1  > snd board2 = board1
    | otherwise = board2

day4 = do
  putStrLn "day4"
  contents <- readFile "../input/day4.txt"
  let input = lines contents
  let numbers = splitOn ',' $ takeWhile (/= "") input
  let boards = do {
      map (splitIntoBlocksOf 5 . filter (/="") . splitOn ' ') 
      $ splitIntoBlocksOf 5 
      $ filter (/= "") 
      $ dropWhile (/= "") 
      input
  }
  let winningBoard = day4a boards numbers
  let loosingBoard = day4b boards numbers
  print (calculateDay4Solution winningBoard numbers)
  print (calculateDay4Solution loosingBoard numbers)
 
  

