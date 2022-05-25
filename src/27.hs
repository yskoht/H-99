group :: [Int] -> [a] -> [[[a]]]
group [] _ = [[]]
group (x:xs) ys = do
  (k, rs) <- combs x ys
  gs <- group xs rs
  return (k:gs)

combs :: Int -> [a] -> [([a], [a])]
combs n xs = combs' n xs [] []

combs' :: Int -> [a] -> [a] -> [a] -> [([a], [a])]
combs' 0 xs ys zs = [(ys, xs ++ zs)]
combs' _ [] ys zs = []
combs' n (x:xs) ys zs = a ++ b
  where
    a = combs' (n-1) xs (ys ++ [x]) zs
    b = combs' n xs ys (zs ++ [x])
