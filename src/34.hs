totient :: Int -> Int
totient 1 = 1
totient m = length [i | i <- [1..m-1], coprime i m]

coprime :: Int -> Int -> Bool
coprime a b = gcd a b == 1