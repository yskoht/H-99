primesR :: Int -> Int -> [Int]
primesR a b = [i | i <- [a..b], isPrime i]

isPrime :: Int -> Bool
isPrime 1 = False
isPrime n = and [n `mod` i /= 0 | i <- [2..n-1], i * i <= n]
