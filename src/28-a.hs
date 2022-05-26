lsort :: [[a]] -> [[a]]
lsort [] = []
lsort (x:xs) = lsort l ++ [x] ++ lsort r
  where
    l = [y | y <- xs, length y < length x]
    r = [y | y <- xs, length y >= length x]

-- sortBy (comparing length)
