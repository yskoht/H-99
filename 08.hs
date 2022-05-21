compress :: Eq a => [a] -> [a]
compress [] = []
compress [x] = [x]
compress (x:y:z)
  | x == y = compress (x:z)
  | otherwise = x:compress (y:z)

-- group
