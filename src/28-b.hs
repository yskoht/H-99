lfsort :: [[a]] -> [[a]]
lfsort [] = []
lfsort all@(x:xs) = lfsort l ++ [x] ++ lfsort r
  where
    t = f x all
    l = [y | y <- xs, f y all < t]
    r = [y | y <- xs, f y all >= t]

f :: [a] -> [[a]] -> Int
f t = length . filter (\a -> length a == length t)

