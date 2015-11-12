safeHead::[a] -> Maybe a
safeHead [] = Nothing
safeHead (a:as) = Just a

safeTail::[a] -> Maybe [a]
safeTail [] = Nothing
safeTail (a:as) = Just as

safeMax:: (Ord a) => [a] -> Maybe a
safeMax [] = Nothing
safeMax (a:as) = 