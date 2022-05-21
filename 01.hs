myLast :: [a] -> a
myLast [] = error ""
myLast [x] = x
myLast (_:xs) = myLast xs

-- head
-- reverse
-- flip
-- const
-- foldr1
