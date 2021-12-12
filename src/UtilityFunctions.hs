module UtilityFunctions where
import Debug.Trace ( trace )

splitIntoBlocksOf n ls
  | n <= 0 || null ls = []
  | otherwise = take n ls : splitIntoBlocksOf n (drop n ls)

split sep ls
  | null ls = []
  | otherwise = takeWhile (/= sep) ls : split sep (drop 1 $ dropWhile (/= sep) ls)

splitEachChar :: [Char] -> [String]
splitEachChar ls
  | null ls = []
  | otherwise = take 1 ls : splitEachChar (drop 1 ls)

splitOn sep = concatMap (split sep)

mapReduce transform reduce init li = foldl reduce init (map transform li)

readInt x = read x :: Int

readInteger x = read x :: Integer

readCharToInt x = read [x] :: Int

rotate = drop <> take

replace n val li = take n li <> [val] <> drop (n + 1) li

replace2d i j val li = take i li <> [replace j val (li !! i)] <> drop (i + 1) li

transformElement n fn li = take n li <> [fn (li !! n)] <> drop (n + 1) li

debug = flip trace
