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

-- Aufgabe 4
