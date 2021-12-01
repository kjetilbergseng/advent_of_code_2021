import Day1

main :: IO ()
main = do
  putStrLn ""
  putStrLn $ if day1a [199, 200, 208, 210, 200, 207, 240, 269, 260, 263] == 7 then "Day 1a OK" else "Failed day1a!"
  putStrLn $ if day1a . day1b [199, 200, 208, 210, 200, 207, 240, 269, 260, 263] == 5 then "Day 1b OK" else "Failed day1b!"
  return ()
