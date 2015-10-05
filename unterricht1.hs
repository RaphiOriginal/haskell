{-# OPTIONS_GHC -Wall #-}
-- Aufgabe 1
switchFirstTwo :: [a] -> [a]
switchFirstTwo (x : y : xs) = y : x : xs
switchFirstTwo a = a

--Aufgabe 2
type Vec = (Int, Int)
--a)
addVec :: Vec -> Vec -> Vec
addVec (a, b) (c, d) = (a + c, b + d)
--b)
addVecOpt :: Vec -> Vec -> Vec
addVecOpt (a, b) (c, d) = (addOpt a c, addOpt b d)

addOpt :: Int -> Int -> Int
addOpt a 0 = a
addOpt 0 b = b
addOpt a b = a + b