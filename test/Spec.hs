import Day1
import Day2
import Day15

main :: IO ()
main = do
  putStrLn ""
  let day1Inp = [199, 200, 208, 210, 200, 207, 240, 269, 260, 263]
  putStrLn $ if day1a day1Inp == 7 then "Day 1a OK" else "Failed day1a!"
  putStrLn $ if (day1a . day1b $ day1Inp) == 5 then "Day 1b OK" else "Failed day1b!"

  let day2Inp = [("forward", " 5"), ("down", " 5"), ("forward", " 8"), ("up", " 3"), ("down", " 8"), ("forward", " 2")]

  putStrLn $ if day2a day2Inp == 150 then "Day 2a OK" else "Failed day2a!"
  putStrLn $ if day2b day2Inp == 900 then "Day 2b OK" else "Failed day2b!"

  day15
  
  return ()
