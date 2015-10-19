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
