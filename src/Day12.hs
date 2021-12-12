module Day12 where
import UtilityFunctions
import Control.Monad (join)
import Data.List ( (\\) )
import Data.Char ( isUpper )

solve visited (li, usedSmallRoomVisit) current
    | current == "end" = 1 -- `debug` intercalate "," (reverse ("end":visited))
    | not $ any (elem current) li = 0
    | otherwise =
        sum
        $ map
        (solve (current:visited) (updateList li usedSmallRoomVisit current visited))
        (join $ map (\\[current]) (filter (elem current) li))

updateList li usedSmallRoomVisit current visited
        | current == "start" = (rs, usedSmallRoomVisit)
        | isUpper (head current) = (rs, usedSmallRoomVisit)
        | current `elem` visited || usedSmallRoomVisit= (filteredList, True)
        | otherwise = (rs, usedSmallRoomVisit)
    where 
        rs = filter (notElem "start") li
        filteredList=filterOutVisitedSmalRooms (current:visited) li

filterOutVisitedSmalRooms [] li = li
filterOutVisitedSmalRooms (v:vs) li
    | isUpper (head v) = filterOutVisitedSmalRooms vs li
    | otherwise = filterOutVisitedSmalRooms vs (filter (notElem v) li)
    
day12 = do
  putStrLn "day12"
  contents <- readFile "../input/day12.txt"
  let input =  map (split '-') (lines contents)
  print $ solve [] (input, True) "start"
  print $ solve [] (input, False) "start"