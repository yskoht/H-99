dropEvery :: [a] -> Int -> [a]
dropEvery [] _ = []
dropEvery xs n =
  if length xs >= n
    then init (take n xs) ++ dropEvery (drop n xs) n
    else xs
