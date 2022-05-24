combinations :: Int -> [a] -> [[a]]
combinations n xs = combinations' n xs []

combinations' :: Int -> [a] -> [a] -> [[a]]
combinations' 0 _ ys = [ys]
combinations' _ [] _ = []
combinations' n (x:xs) ys = a ++ b
  where
    a = combinations' (n-1) xs (ys ++ [x])
    b = combinations' n xs ys
