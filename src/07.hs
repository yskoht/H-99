data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> [a]
flatten (Elem e) = [e]
flatten (List []) = []
flatten (List (x:xs)) = flatten x ++ flatten (List xs)

-- concatMap
