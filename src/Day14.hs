module Day14 where

import qualified Data.Map as Map
import Data.Maybe (fromMaybe)

getVal key map = fromMaybe 0 (Map.lookup key map)

getRule key map = fromMaybe "" (Map.lookup key map)

update [] rules map mapOut = mapOut
update (key : keys) rules map mapOut
  | getVal key map == 0 = update keys rules map mapOut
  | otherwise = update keys rules map (addToMap (getRule key rules) (getVal key map) mapOut)

changPos s = head s : s !! 2 : [s !! 1]

addToMap template val mapOut
  | length template < 2 = mapOut
  | otherwise = addToMap (drop 1 template) val (Map.insertWith (+) (take 2 template) val mapOut)

performSteps step steps keys rules mapOut
  | step == steps = mapOut
  | otherwise = performSteps (step + 1) steps keys rules (update keys rules mapOut Map.empty)

getSum hashMap keys template = (-) <$> maximum <*> minimum $ [getVal key sumMap | key <- Map.keys sumMap]
  where
    startMap = Map.insertWith (+) [last template] 1 Map.empty
    sumMap = foldl (\m k -> Map.insertWith (+) (take 1 k) (getVal k hashMap) m) startMap keys

day14 = do
  putStrLn "day14"
  contents <- readFile "../input/day14.txt"
  let template = head $ lines contents
  let rules = map (changPos . filter (\x -> x /= ' ' && x /= '-' && x /= '>')) (drop 2 $ lines contents)
  let keys = map (takeWhile (/= ' ')) (drop 2 $ lines contents)

  let ruleMap = Map.fromList (zip keys rules)
  let startMap = addToMap template 1 Map.empty

  print $ getSum (performSteps 0 10 keys ruleMap startMap) keys template
  print $ getSum (performSteps 0 40 keys ruleMap startMap) keys template
