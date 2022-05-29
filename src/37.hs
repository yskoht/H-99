import Data.List

primeFactorsMult :: Int -> [(Int, Int)]
primeFactorsMult n = map f xs
  where
    xs = group . sort $ primeFactors n
    f a = (head a, length a)

primeFactors :: Int -> [Int]
primeFactors 1 = []
primeFactors n = p : primeFactors (n `div` p)
  where
    p = find' f [2..n]
    f a = n `mod` a == 0 && isPrime a

find' :: (a -> Bool) -> [a] -> a
find' f xs = head [x | x <- xs, f x]

isPrime :: Int -> Bool
isPrime 1 = False
isPrime n = and [n `mod` i /= 0 | i <- [2..n-1], i * i <= n]

phi :: Int -> Int
phi m = foldl f 1 xs
  where
    xs = primeFactorsMult m
    f :: Int -> (Int, Int) -> Int
    f acc (p', m') = acc * (p' - 1) * p' ^ (m' - 1)
