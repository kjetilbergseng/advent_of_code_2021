{-# LANGUAGE ParallelListComp #-}

module Day4 where
import Data.List (transpose)
import Control.Monad (join)
import UtilityFunctions (split)

data Result = Result { board :: [String]
                     , pickNumber :: Int
                     } deriving Show

mask boards numbers=[[[number `elem` numbers | number<-row] | row <-board] | board <- boards]

checkForWinningBoard boards mask = filter (/=[]) [[board | rowMask<-boardMask, and rowMask ] | board <- boards | boardMask<-mask]

flattenBoard board= join $ head $ head board

bingo i boards numbers
    | board /= [] = Result (flattenBoard board) i
    | otherwise = bingo (i+1) boards numbers
    where board = checkForWinningBoard boards $ mask boards (take i numbers)   

remainingNumbers board numbers=[number | number <- board, number `notElem` numbers]

getNthNumber n numbers= read $ last (take n numbers)

getSumOfRemaining result numbers = 
    sum 
    $ map read 
    $ remainingNumbers (board result) 
    $ take (pickNumber result) numbers

calculateScore result numbers = getNthNumber (pickNumber result) numbers * getSumOfRemaining result numbers

findWinner boards numbers
    | pickNumber row < pickNumber col = row 
    | otherwise = col
    where row = bingo 1 boards numbers 
          col = bingo 1 (map transpose boards) numbers

findLooser boards numbers = foldl1 (getLooser numbers) [findWinner [board] numbers | board <- boards]

getLooser numbers result1 result2 
    | pickNumber result1  > pickNumber result2 = result1
    | otherwise = result2

day4 = do
  putStrLn "day4"
  contents <- readFile "../input/day4.txt"
  let input = lines contents
  let numbers = split ',' (head input)
  let boards =  tail $ map (map words) $ split "" input
  let winningBoard = findWinner boards numbers
  let loosingBoard = findLooser boards numbers
  print (calculateScore winningBoard numbers)
  print (calculateScore loosingBoard numbers)