type Coords = (Integer, Integer)
type Coords2 = (Integer, Integer, Integer)

moveSubmarine :: Coords -> ([Char], Integer) -> (Integer, Integer)
moveSubmarine (horizontal, depth) (command, value)
      | command == "forward" = (horizontal + value, depth)
      | command == "down" = (horizontal, depth + value)
      | command == "up" = (horizontal, depth - value)
      | otherwise = (horizontal, depth)

moveSubmarine2 :: Coords2 -> ([Char], Integer) -> (Integer, Integer, Integer)
moveSubmarine2 (horizontal, depth, aim) (command, value)
      | command == "forward" = (horizontal + value, depth + (aim*value), aim)
      | command == "down" = (horizontal, depth, aim + value)
      | command == "up" = (horizontal, depth, aim - value)
      | otherwise = (horizontal, depth, aim)

main :: IO ()
main = do
  input <- readFile "day2.txt"
  let moves = [(x, y) | linii <- lines input, let s = span (/= ' ') linii, let x = fst s, let y = read (snd s)]
  --print moves
  let (h1, d1) = foldl moveSubmarine (0, 0) moves
  let (h2, d2, aim) = foldl moveSubmarine2 (0, 0, 0) moves
  print (h1*d1, h2*d2)


