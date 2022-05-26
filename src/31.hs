isPrime :: Int -> Bool
isPrime 1 = False
isPrime n = and [n `mod` i /= 0 | i <- [2..n-1], i * i <= n]
