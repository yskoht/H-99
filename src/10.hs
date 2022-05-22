pack :: Eq a => [a] -> [[a]]
pack [] = []
pack (x:xs) =
  let (first, rest) = span (== x) xs
  in (x:first) : pack rest

encode :: Eq a => [a] -> [(Int, a)]
encode = map (\x -> (length x, head x)) . pack
