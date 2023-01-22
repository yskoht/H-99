-- {-# OPTIONS -Wall -Werror #-}

data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show, Eq)

construct :: [Int] -> Tree Int
construct = foldl construct' Empty

construct' :: Tree Int -> Int -> Tree Int
construct' Empty n = Branch n Empty Empty
construct' (Branch r a b) n
  | n < r = Branch r (construct' a n) b
  | otherwise = Branch r a (construct' b n)


symmetric :: Tree a -> Bool
symmetric Empty = True
symmetric (Branch _ a b) = mirror a b

mirror :: Tree a -> Tree a -> Bool
mirror Empty Empty = True
mirror Empty _ = False
mirror _ Empty = False
mirror (Branch _ a1 a2) (Branch _ b1 b2) = mirror a1 b2 && mirror a2 b1
