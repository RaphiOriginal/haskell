incAll :: [Int] -> [Int]
incAll = \a -> map (\b -> b + 1) a

addToAll :: Int -> [Int] -> [Int]
addToAll = \a -> \b -> map(\x -> x + a) b

keepOld :: [Int] -> [Int]
keepOld = \a -> filter(\b -> b > 90) a

dropShort :: [String] -> [String]
dropShort = \s -> filter(\a -> length a /= 1) s

