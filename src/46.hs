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

table :: (Bool -> Bool -> Bool) -> IO()
table f = do
  forM_ xs $ \x -> do
    let (a, b) = x
    print $ show a ++ " " ++ show b ++ " " ++ show (f a b)
  where
    xs = [(a, b) | a <- [True, False], b <- [True, False]]
