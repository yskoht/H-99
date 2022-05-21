data EncodeModified a = Single a | Multiple Int a deriving Show

encodeDirect :: Eq a => [a] -> [EncodeModified a]
encodeDirect [] = []
encodeDirect (x:xs) =
  let (first, rest) = span (==x) xs
  in encode (x:first) : encodeDirect rest
    where
      encode :: Eq a => [a] -> EncodeModified a
      encode first =
        case len of
          1 -> Single val
          _ -> Multiple len val
        where
          len = length first
          val = head first
