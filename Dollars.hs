import Data.Char

-- a)
inc :: Int -> Int
inc i = i + 1

bob = filter even $ map inc [1,2,3,4]

-- b)

b1 = sum $ filter even [1, 2, 3]
b2 = map even $ map inc $ filter (>2) [1, 2, 3]
b3 = map toUpper $ filter (not.isSpace) "abc def"

-- c)

fs = [(+1),(*3),(/2)]
ns = [1, 2, 3]

magicFunction [] _ = []
magicFunction _ [] = []
magicFunction (f:fs) (n:ns) = f (fromIntegral n) : magicFunction fs ns