import Data.List.Split

parseInput :: String -> [Int]
parseInput input = map read $ splitOn "," input

countDays :: [Int] -> [Int]
countDays fishList = map count [0..8]
  where count t = length $ filter (==t) fishList

dayPassed :: [Int] -> [Int]
dayPassed frequencyList = map (\day -> count (day+1)) [0..5] ++ [count 0 + count 7, count 8, count 0]
  where count day = frequencyList!!day

main :: IO ()
main = do
  input <- readFile "day6.txt"
  let stats = iterate dayPassed $ countDays $ parseInput input
  print (sum $ stats!!80, sum $ stats!!256)
