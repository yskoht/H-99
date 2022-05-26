myGCD :: Int -> Int -> Int
myGCD m 0 = abs m
myGCD m n = myGCD m' n'
  where
    n' = m `mod` n
    m' = n
