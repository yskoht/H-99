import Control.Monad

and' :: Bool -> Bool -> Bool
and' True True = True
and' _ _ = False

or' :: Bool -> Bool -> Bool
or' False False = False
or' _ _ = True

not' :: Bool -> Bool
not' False = True
not' True = False

nand' :: Bool -> Bool -> Bool
nand' a b = not' $ and' a b

nor' :: Bool -> Bool -> Bool
nor' a b = not' $ or' a b

xor' :: Bool -> Bool -> Bool
xor' a b = (a `and'` not' b) `or'` (not' a `and'` b)

impl' :: Bool -> Bool -> Bool
impl' a b = not' a `or'` b

equ' :: Bool -> Bool -> Bool
equ' a b = not $ a `xor'` b

tablen :: Int -> ([Bool] -> Bool) -> IO()
tablen n f =
  forM_ xs $ \x -> do
    print $ show x ++ " " ++ show (f x)
  where
    xs = tablen' n

-- replicateM
tablen' :: Int -> [[Bool]]
tablen' 0 = [[]]
tablen' n = do
  xs <- tablen' (n - 1)
  t <- [True, False]
  return (t : xs)
