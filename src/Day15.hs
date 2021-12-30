module Day15 where

import UtilityFunctions

rightDown q [] _ _  =  []
rightDown q (x:xs) [] []  =  q + x : rightDown (q + x) xs [] []
rightDown q (x:xs) (y:ys) []  =  qy : rightDown qy xs ys []-- # show qq
    where qy= min (q+x) (y+x)
rightDown q (x:xs) [] (z:zs)  =  qz : rightDown qz xs [] zs
    where qz= min (q+x) z
rightDown q (x:xs) (y:ys) (z:zs)  =  qyz : rightDown qyz xs ys zs
    where qyz= min3 (q+x) (y+x) z

min3 a b c= min c $ min a b

getPathrightDown _ [] _ = []
getPathrightDown [] (y:ys) [] = q0 : getPathrightDown q0 ys []
    where
    q0=rightDown 0 y [] []
getPathrightDown [] (y:ys) (z:zs) = q0 : getPathrightDown q0 ys zs
    where
    q0=rightDown 0 y [] z
getPathrightDown x (y:ys) [] = q : getPathrightDown q ys []
    where
    q=rightDown (head x) y x []
getPathrightDown x (y:ys) (z:zs) = q : getPathrightDown q ys zs
    where
    q=rightDown (head x) y x z

expandDir1Impl i n li output
  | i==n = output
  | otherwise = expandDir1Impl (i+1) n li (output <> map (+i) li)

expandDir1 n li = expandDir1Impl 0 n li []

expandDir2Impl i n li output
  | i==n = tail output
  | otherwise = expandDir2Impl (i+1) n li (output <> map (map (+i)) li)

expandDir2 n li = expandDir2Impl 0 n li [[]]

expandInput n li = map (map (\x -> if x>9 then x-9 else x)) (expandDir2 n (map (expandDir1 n) li))

reverse2d li= reverse $ map reverse li
performIteration i n input output
  | i==n*2 = output
  | even i = performIteration (i+1) n input (getPathrightDown [] input output)
  | otherwise = performIteration (i+1) n input (reverse2d $ getPathrightDown [] rinput routput)
  where
    routput = reverse2d output
    rinput = replace2d 0 0 (head $ head routput) (reverse2d input)

solve n input =
  head . head . reverse2d $ performIteration 0 n (replace2d 0 0 0 input) []

day15 = do
  putStrLn "day15"
  contents <- readFile "../input/day15.txt"
  let input = map (map readInt . splitEachChar) (lines contents)
  print $ solve 10 input
  print $ solve 10 (expandInput 5 input)


