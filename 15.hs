repli :: [a] -> Int -> [a]
repli xs n = concatMap (replicate n) xs

-- flip :: (a -> b -> c) -> b -> a -> c
-- repli = flip $ concatMap . replicate
