myButLast :: [a] -> a
myButLast [] = error "error"
myButLast [x] = error "error"
myButLast (x:xs) = if length xs == 1 then x else myButLast xs
