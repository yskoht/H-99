import System.Random.Shuffle (shuffleM)

diffSelect :: Int -> Int -> IO [Int]
diffSelect n m = do
  arr <- shuffleM [1..m]
  return $ take n arr
