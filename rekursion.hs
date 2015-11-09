factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- Aufgabe 1
{-
Es springt immer zwischen - und + Werten umher richtung unendlich und - unendlich
Factorial ist nicht definiert für negative Zahlen
-}

-- Aufgabe 2
countDown :: Int -> [Int]
countDown 0 = [0]
countDown n = [n] ++ countDown (n - 1)

-- Aufgabe 3
countUp :: Int -> [Int]
countUp 0 = [0]
countUp n = countUp (n - 1) ++ [n]

-- Aufgabe 4
countDownUp :: Int -> [Int]
countDownUp 0 = [0]
countDownUp n = [n] ++ countDownUp (n - 1) ++ [n]

-- Arbeitsblatt 2

-- Aufgabe 1
len :: [a] -> Int
len [] = 0
len (x:xs) = 1 + len xs

-- Aufgabe 2
allTrue :: [Bool] -> Bool
allTrue [] = True
allTrue (x:xs) = x && allTrue xs

-- Aufgabe 3
sublist :: Int -> Int -> [a] -> [a]
sublist _ 0 _ = []
sublist _ _ [] = error "need more chars from enpty list"
sublist 0 b (x:xs) = x : sublist 0 (b-1) xs
sublist a b (_:xs) sublist (a-1) b xs

{-
sublist 0 x as = sublist' (x) [] as
sublist x y (a:as) = sublist (x-1) y as

sublist' :: Int -> [a] -> [a] -> [a]
sublist' x y [] = y
sublist' 0 y _ = y
sublist' x y (z:zs) = sublist' (x - 1) (y ++ [z]) zs
-}

(+++) :: [a] -> [a] -> [a]
(+++) [] a = a
(+++) (x:xs) a =  x : (xs +++ a)

(++++) :: [a] -> [a] -> [a]
[] ++++ as = as
(x:xs) ++++ as = x : (xs ++++ as)