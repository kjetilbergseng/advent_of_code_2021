module Day12 where
import UtilityFunctions
import Control.Monad (join)
import Data.List
import Data.Char

solve2 :: [[Char]] -> ([[[Char]]],Bool) -> [Char] -> Int
solve2 visited (li, usedSmallRoomVisit) current
    | current == "end" = 1 -- `debug` intercalate "," (reverse ("end":visited))
    | not $ any (elem current) li = 0
    | otherwise =
        sum
        $ map
        (solve2 (current:visited) (updateList li usedSmallRoomVisit current visited))
        (join $ map (\\[current]) (filter (elem current) li))

updateList li usedSmallRoomVisit current visited
        | current == "start" = (rs, usedSmallRoomVisit)
        | isUpper (head current) = (rs, usedSmallRoomVisit)
        | current `elem` visited || usedSmallRoomVisit= (filteredList, True)
        | otherwise = (rs, usedSmallRoomVisit)
    where 
        rs = filter (notElem "start") li
        filteredList=filterOutVisitedSmalRooms (current:visited) li

filterOutVisitedSmalRooms v li
    | null v = li
    | isUpper (head (head v)) = filterOutVisitedSmalRooms (tail v) li
    | otherwise = filterOutVisitedSmalRooms (tail v) (filter (notElem (head v)) li)
    
day12 = do
  putStrLn "day12"
  contents <- readFile "../input/day12.txt"
  let input =  map (split '-') (lines contents)
  print $ solve2 [] (input, True) "start"
  print $ solve2 [] (input, False) "start"