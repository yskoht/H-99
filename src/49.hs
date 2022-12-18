gray :: Int -> [String]
gray 0 = []
gray 1 = ["0", "1"]
gray n = u ++ b
  where
    u = map ("0" ++) u'
    b = map ("1" ++) b'
    u' = gray (n - 1)
    b' = reverse $ gray (n - 1)
