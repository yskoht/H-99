rotate :: [a] -> Int -> [a]
rotate xs n = drop k xs ++ take k xs
  where
    len = length xs
    k = (len + n) `mod` len
