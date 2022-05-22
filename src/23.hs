import System.Random
import Control.Monad (replicateM)

rndSelect :: [a] -> Int -> IO [a]
rndSelect [] _ = return []
rndSelect l n
  | n < 0 = error ""
  | otherwise = do
    let len = length l - 1
    let rand = getStdRandom $ randomR (0, len)
    pos <- replicateM n rand
    return [l !! p | p <- pos]
