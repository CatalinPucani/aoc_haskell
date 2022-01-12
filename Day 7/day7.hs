import Data.List.Split

parseInput :: String -> [Int]
parseInput input = map read $ splitOn "," input

calculateFuel1 :: [Int] -> Int -> Int
calculateFuel1 cp kpos = sum $ map (\c -> abs (kpos-c)) cp

calculateFuel2 :: [Int] -> Int -> Int 
calculateFuel2 cp kpos = sum $ map(\c -> div ((abs(kpos - c))*(abs(kpos - c)) + abs(kpos - c) ) 2) cp

main :: IO ()
main = do
  input <- readFile "day7.txt"
  let inputBun = parseInput input
  let minim = minimum inputBun
  let maxim = maximum inputBun
  print( minimum $ map (calculateFuel1 inputBun) [minim..maxim], minimum $ map (calculateFuel2 inputBun) [minim..maxim])
