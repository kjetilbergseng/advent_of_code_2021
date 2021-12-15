module Day14 where

import qualified Data.Map as Map
import Data.Maybe (fromMaybe)

getVal key map = fromMaybe 0 (Map.lookup key map)

getTransformations key map = fromMaybe "" (Map.lookup key map)

update [] transformations mapIn mapOut = mapOut
update (key : keys) transformations mapIn mapOut
  | getVal key mapIn == 0 = update keys transformations mapIn mapOut
  | otherwise = update keys transformations mapIn (addToMap (getTransformations key transformations) (getVal key mapIn) mapOut)

changPos s = head s : s !! 2 : [s !! 1]

addToMap input val mapOut
  | length input < 2 = mapOut
  | otherwise = addToMap (drop 1 input) val (Map.insertWith (+) (take 2 input) val mapOut)

performSteps step steps keys transformations mapOut
  | step == steps = mapOut
  | otherwise = performSteps (step + 1) steps keys transformations (update keys transformations mapOut Map.empty)

solve hashMap keys input = (-) <$> maximum <*> minimum $ [getVal key sumMap | key <- Map.keys sumMap]
  where
    startMap = Map.singleton [last input] 1
    sumMap = foldl (\m k -> Map.insertWith (+) (take 1 k) (getVal k hashMap) m) startMap keys

day14 = do
  putStrLn "day14"
  contents <- readFile "../input/day14.txt"
  let input = head $ lines contents
  let keys = map (takeWhile (/= ' ')) (drop 2 $ lines contents)
  let values = map (changPos . filter (\x -> x /= ' ' && x /= '-' && x /= '>')) (drop 2 $ lines contents)

  let transformationMap = Map.fromList (zip keys values)
  let startMap = addToMap input 1 Map.empty

  print $ solve (performSteps 0 10 keys transformationMap startMap) keys input
  print $ solve (performSteps 0 40 keys transformationMap startMap) keys input
