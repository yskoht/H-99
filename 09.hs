pack :: Eq a => [a] -> [[a]]
pack = pack' [] []


pack' :: Eq a => [[a]] -> [a] -> [a] -> [[a]]
pack' acc cur [] = acc ++ [cur]
pack' acc [] (x:xs) = pack' acc [x] xs
pack' acc cur (x:xs)
  | head cur == x = pack' acc (cur ++ [x]) xs
  | otherwise = pack' (acc ++ [cur]) [] (x:xs)

-- span
-- takeWhile, dropWhile
