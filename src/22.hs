range :: Int -> Int -> [Int]
range x y
  | x == y = [y]
  | otherwise = x : range (x+1) y
