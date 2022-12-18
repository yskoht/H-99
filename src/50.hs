-- {-# OPTIONS -Wall -Werror #-}
import Data.List

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show)
type Pair = (String, Int)


huffman :: [(String, Int)] -> [(String, String)]
huffman xs = parse $ head $ huffmanTree $ map leaf xs


huffmanTree :: [Tree Pair] -> [Tree Pair]
huffmanTree xs
  | length xs <= 1 = xs
  | otherwise = huffmanTree nxs
  where
    f:s:xss = sortTree xs
    t = addTree f s
    nxs = xss ++ [t]


leaf :: a -> Tree a
leaf x = Node x Empty Empty


sortTree :: [Tree Pair] -> [Tree Pair]
sortTree xs = sortBy compareTree xs
  where
    compareTree :: Tree Pair -> Tree Pair -> Ordering
    compareTree (Node (_, x) _ _) (Node (_, y) _ _) = compare x y
    compareTree _ _ = EQ


addTree :: Tree Pair -> Tree Pair -> Tree Pair
addTree Empty Empty = Empty
addTree Empty a = a
addTree a Empty = a
addTree ta@(Node (sa, a) _ _) tb@(Node (sb, b) _ _) = Node ("(" ++ sa ++ "+" ++ sb ++ ")", a + b) ta tb


parse :: Tree Pair -> [(String, String)]
parse t = _parse t ""

_parse :: Tree Pair -> String -> [(String, String)]
_parse Empty _ = []
_parse (Node (s, _) Empty Empty) c = [(s, c)]
_parse (Node _ a b) c = (_parse a (c ++ "0")) ++ (_parse b (c ++ "1"))

{-
  huffman [("a",45),("b",13),("c",12),("d",16),("e",9),("f",5)]

  huffmanTree
  [
    Node ("(a+((c+b)+((f+e)+d)))",100)
      (Node ("a",45) Empty Empty)
      (Node ("((c+b)+((f+e)+d))",55)
        (Node ("(c+b)",25)
          (Node ("c",12) Empty Empty)
          (Node ("b",13) Empty Empty)
        )
        (Node ("((f+e)+d)",30)
          (Node ("(f+e)",14)
            (Node ("f",5) Empty Empty)
            (Node ("e",9) Empty Empty)
          )
          (Node ("d",16) Empty Empty)
        )
      )
  ]

  parse
  [("a","0"),("c","100"),("b","101"),("f","1100"),("e","1101"),("d","111")]
-}