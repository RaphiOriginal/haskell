-- 1. a)
threeEquals :: Int -> Int -> Int -> Bool
threeEquals a b c = a == b && b == c

-- 1. b)
fourEquals1 :: Int -> Int -> Int -> Int -> Bool
fourEquals1 a b c d = a == b && c == d && b == c

fourEquals2 :: Int -> Int -> Int -> Int -> Bool
fourEquals2 a b c d = (threeEquals a b c) && (threeEquals b c d)

-- bei fourEquals1 wird ein vergleich weniger gemacht als bei fourEquals2 (zumindest wenn true zurück kommt)

-- 1. c)
averageThree :: Int -> Int -> Int -> Double
averageThree a b c = (fromIntegral (a + b + c)) / 3

-- 1. d)
xor :: Bool -> Bool -> Bool
xor a b = not (a == b)

-- 2. a)
data Op = Add | Sub

calc :: Op -> Int -> Int ->Int
calc Add a b = a + b
calc Sub a b = a - b

-- 2. b)
data Burger = BigMac | CheeseRoyal deriving (Show)
data Size = Small | Large deriving (Show)

type Order = (Burger, Size)

price :: Order -> Int
price (BigMac, Small) = 10
price (CheeseRoyal, Small) = 11
price (BigMac, Large) = 12
price (CheeseRoyal, Large) = 13

-- 3. a)
swap :: (a, b) -> (b, a)
swap (a, b) = (b, a)
-- alle Typen möglich

-- 3. b)
pair :: a -> b -> (a, b)
pair a b = (a, b)
-- alle Typen möglich

-- 3. c)
-- weiss nicht mehr wie man das richtig macht... =)
-- double :: a => Num a -> a
double a = a * 2
-- nur Numerische typen möglich

-- 3. d)
crazy :: (Bool, Char, (Bool, Bool)) -> Bool
crazy (a, '&', (c, True)) = not (a && c)
crazy (a, '|', (c, True)) = not (a || c)
crazy (a, '&', (c, False)) = a && c
crazy (a, '|', (c, False)) = a || c
crazy (a, _, (c, _)) = not a && not c
-- nur Boolean möglich bis auf die fallunterscheidung mit dem Char, müsste für jeden char eine unterscheidung geben!

-- 3. e)
-- es scheint eine funktion übergeben worden sein die dann mit x ausgeführt wird und danach nochmal mit dem resultat davon!

-- 4. a)
f1 :: Int -> Int
f1 a = a

-- 4. b)
f2 :: (Int, Bool) -> Int
f2 (a, b) = a

-- 4. c)
f3 :: a -> (a, Int)
f3 a = (a, 1)

-- 4. d)
-- f4 :: a -> b
-- f4 a = []
-- Ich gibs uf..

-- 4. e)
f5 :: a -> (a -> b) -> b
f5 a c = c a
