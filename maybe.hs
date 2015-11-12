safeHead::[a] -> Maybe a
safeHead [] = Nothing
safeHead (a:as) = Just a

safeTail::[a] -> Maybe [a]
safeTail [] = Nothing
safeTail (a:as) = Just as

safeMax:: (Ord a) => [a] -> Maybe a
safeMax [] = Nothing
safeMax (x:[]) = Just x
safeMax (a:b:as) | a > b = safeMax (a:as)
                 | otherwise = safeMax (b:as) 