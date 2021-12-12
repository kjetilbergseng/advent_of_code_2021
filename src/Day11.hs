module Day11 where
import UtilityFunctions
import Data.Matrix
import Data.Data (dataTypeConstrs)

updateElem add i j (m, flashes)
  | i<1 || i > nrows m || j<1 || j > ncols m = (m, flashes)
  | getElem i j m == 0 = (m, flashes)
  | getElem i j m >9 = 
        updateElem 1 (i-1) (j-1)
        (updateElem 1 i (j-1)
        (updateElem 1 (i+1) (j-1)
        (updateElem 1 (i-1) j
        (updateElem 1 (i+1) j
        (updateElem 1 (i-1) (j+1)
        (updateElem 1 i (j+1)
        (updateElem 1 (i+1) (j+1) (setElem 0 (i, j) m, flashes+1) )))))))                                                                          
  | otherwise = (setElem (add + getElem i j m) (i, j) m, flashes)

updateAllElems i j (m,flashes)
  | j > ncols m = (m,flashes)
  | i == nrows m = updateAllElems 1 (j+1) (updateElem 0 i j (m,flashes))  
  | otherwise = updateAllElems (i+1) j (updateElem 0 i j (m,flashes))  

done i j m 
  | j > ncols m = True
  | i>nrows m = done 1 (j+1) m
  | getElem i j m >9 = False
  | otherwise = done (i+1) j m
  
update (m, flashes)
  | done 1 1 m = (m, flashes) 
  | otherwise = update $ updateAllElems 1 1 (m, flashes)

runFor day days (m,flashes)
  | day==days = flashes
  | otherwise = runFor (day+1) days (update (fmap (+1) m, flashes))

allZero i j m 
  | j > ncols m = True
  | i>nrows m = allZero 1 (j+1) m
  | getElem i j m >0 = False
  | otherwise = allZero (i+1) j m

runUntilAllZero day m
  | allZero 1 1 m = day
  | otherwise = runUntilAllZero (day+1) (fst $ update (fmap (+1) m, 0))

day11 = do
  putStrLn "day11"
  contents <- readFile "../input/day11.txt"
  let input =  fromLists $ map (map readInt . splitEachChar) (lines contents)
  print $ runFor 0 100 (input,0)
  print $ runUntilAllZero 0 input