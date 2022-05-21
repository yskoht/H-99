data EncodeModified a = Single a | Multiple Int a deriving Show

pack :: Eq a => [a] -> [[a]]
pack [] = []
pack (x:xs) =
  let (first, rest) = span (== x) xs
  in (x:first) : pack rest

encode :: Eq a => [a] -> [(Int, a)]
encode = map (\x -> (length x, head x)) . pack

encodeModified :: Eq a => [a] -> [EncodeModified a]
encodeModified = map modify . encode
  where
    modify (1, x) = Single x
    modify (n, x) = Multiple n x
