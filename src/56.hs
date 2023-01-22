-- {-# OPTIONS -Wall -Werror #-}

data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show, Eq)

symmetric :: Tree a -> Bool
symmetric Empty = True
symmetric (Branch _ a b) = mirror a b

mirror :: Tree a -> Tree a -> Bool
mirror Empty Empty = True
mirror Empty _ = False
mirror _ Empty = False
mirror (Branch _ a1 a2) (Branch _ b1 b2) = mirror a1 b2 && mirror a2 b1
