import Control.Monad (forM)
import System.Random

rndPermu :: [a] -> IO [a]
rndPermu xs = do
  ys <- permu xs
  i <- randomRIO (0, length ys - 1)
  return $ ys !! i

pick :: [a] -> Int -> (a, [a])
pick xs i = (xs !! i, take i xs ++ drop (i+1) xs)

permu :: [a] -> IO [[a]]
permu [] = return [[]]
permu xs = do
  ps <- forM [0..length xs - 1] $ \i -> do
    let (y, ys) = pick xs i
    zs <- permu ys
    let ts = (++) <$> [[y]] <*> zs
    return ts
  return $ concat ps
