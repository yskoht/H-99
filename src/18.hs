slice :: [a] -> Int -> Int -> [a]
slice xs l r = drop (l-1) $ take r xs