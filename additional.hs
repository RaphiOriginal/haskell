-- Aufgabe 1
f01 :: [[a]] -> [a]
f01 ([x, y]) = x ++ y

f02 :: Num a => [(a, a)] -> a
f02 [(x, y)] = x * y

f03 :: [a] -> (a, a, a)
f03 (x : [y, z]) = (x, y, z)

f04 :: [[a]] -> (a, a, a)
f04 [x : [y, z]] = (x, y, z)

f05 :: (a, a, [a]) -> [a]
f05 (x, y, z) = x : y : z

f06 :: (a, [a], [[a]]) ->[[a]]
f06 (x, y, z) = (x : y) : z

f07 :: (a, [a], [a]) -> [a] 
f07 (x, [y], z) = x : y : z

f08 :: (a, a, a) -> [a]
f08 (x, y, z) = x : y : [z]

f09 :: [[[a]]] -> (a, [a], [[[a]]])
f09 ([[x]] : [y] : z) = (x, y, z)