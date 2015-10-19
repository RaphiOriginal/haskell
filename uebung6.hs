-- Aufgabe 1
-- a)
maxl :: (Ord a) => [a] -> a
maxl (x:[]) = x -- m0
maxl (x:y:xs) = maxl (maxl' x y : xs) -- m1
maxl' :: (Ord a) => a -> a -> a
maxl' x y -- m2
     | x < y = y
     | otherwise = x

-- b)
{-
maxl [2, 5, 1]				-- using m1
~> maxl (maxl' 2 5 : [1])	-- using m2
~> maxl (5 : [1])			-- using list concat
~> maxl [5,1]				-- using m1
~> maxl (maxl' 5 1 : [])	-- using m2
~> maxl (5 : [])			-- using list concat
~> maxl [5]
~> 5
-}

-- Aufgabe 2
--a)
reverse' :: [a] -> [a]
reverse' (x:[]) = [x]  -- r0
reverse' (a:as) = reverse' as ++ [a] -- r1

--b)
{-
reverse' [1,2,3]				-- using r1
~> reverse' [2,3] ++ [1]		-- using r1
~> reverse' [3] ++ [2] ++ [1]	-- using r0
~> [3] ++ [2] ++ [1]			-- using list concat
~> [3,2] ++ [1]					-- using list concat
~> [3,2,1]
-}

-- Aufgabe 3
alternate :: [a] -> [a] -> [a]
alternate x [] = x
alternate [] x = x
alternate (x:xs) (y:ys) = x : y : alternate xs ys

-- Aufgabe 4 (optional)
{-
Die fibonacci-Reihe, man übergibt die Anzahl der Runden die die Reihe
durchlaufen soll und erhält die n-te zahl der Reihe. fibonacci' ist dann blos eine Hilfsfunktion
-}
fibonacci :: Int -> Int
fibonacci r = fibonacci' 1 1 r

fibonacci' :: Int -> Int -> Int -> Int
fibonacci' _ b 0 = b
fibonacci' a b r =  fibonacci' b (a + b) (r - 1)

{-
Die fibonacci-Reihe, man übergibt die Anzahl der Runden die die Reihe
durchlaufen soll und erhält eine Liste mit allen fibonacci zahlen bis
zur angegebenen Runde. fibonacciList' ist nur eine Hilfsfunktion
-}
fibonacciList :: Int -> [Int]
fibonacciList r = [1] ++ fibonacciList' 1 1 r

fibonacciList' :: Int -> Int -> Int -> [Int]
fibonacciList' _ b 0 = [b]
fibonacciList' a b r = [b] ++ fibonacciList' b (a + b) (r -1)
