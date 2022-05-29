primesR :: Int -> Int -> [Int]
primesR a b = [i | i <- [a..b], isPrime i]

isPrime :: Int -> Bool
isPrime 1 = False
isPrime n = and [n `mod` i /= 0 | i <- [2..n-1], i * i <= n]

goldbach :: Int -> (Int, Int)
goldbach n = head [snd i | i <- xs, fst i == n]
  where
    ps = primesR 1 n
    xs = f <$> ps <*> ps
    f a b = (a + b, (a, b))

goldbachList :: Int -> Int -> [(Int, Int)]
goldbachList a b = [goldbach i | i <- [a..b], i > 2 && even i]

goldbachList' :: Int -> Int -> Int -> [(Int, Int)]
goldbachList' a b n = [x | x@(a', b') <- xs, a' >= n && b' >= n]
  where
    xs = goldbachList a b
