insertAt :: a -> [a] -> Int -> [a]
insertAt e [] _ = [e]
insertAt e xs 1 = e : xs
insertAt e (x:xs) n = x : insertAt e xs (n-1)
