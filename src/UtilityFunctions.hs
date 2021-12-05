module UtilityFunctions where

splitIntoBlocksOf n ls
    | n <= 0 ||null ls = []
    | otherwise = take n ls:splitIntoBlocksOf n (drop n ls)

split sep ls
    | null ls = []
    | otherwise = takeWhile (/= sep) ls:split sep (drop 1 $ dropWhile (/= sep) ls)

splitOn sep = concatMap (split sep)

mapReduce transform reduce init li = foldl reduce init (map transform li)