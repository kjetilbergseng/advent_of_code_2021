import Day1

main :: IO ()
main = do
  putStrLn ""
  putStrLn $ if day1a [1721, 979, 1010, 366, 299, 675, 1456] 2020 == 514579 then "Day 1a OK" else "Failed day1a!"
  putStrLn $ if day1b [1721, 979, 1010, 366, 299, 675, 1456] 2020 == 241861950 then "Day 1b OK" else "Failed day1b!"
  return ()
