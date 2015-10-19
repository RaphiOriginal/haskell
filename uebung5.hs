-- Aufgabe 1
--a)
{-
Präzendenz : Operator
10         : ((f a) b) links;
9          : (.) rechts;
8          : (^) rechts;
7          : (*) links; (/) links;
6          : (+) links; (-) links;
5          : (:) rechts; (++) rechts; 
4          : (==); (/=); (<=); (>=); (>); (<); 
3          : (&&) rechts;
2          : (||) rechts;
1          : 
-}
--b)
-- ((((1 + (2 ^ 3)) == 6) && ((3 / 4) < 12)) || (snd(1, True)))
-- (((3:) []) == ((map (*5))[(2 ^ (4 ^ 6))]))

-- Aufgabe 2
--a)
curry1 :: ((a,b) -> c) -> a -> b -> c
curry1 f a b = f (a,b)

--b)
uncurry1 :: (a -> b -> c) -> (a,b) -> c
uncurry1 a (b,c) = a b c

-- Aufgabe 3
--a)b)
flip1 :: (b -> a -> c) -> a -> b -> c
flip1 f a b = f b a

flipl :: (b -> a -> c) -> a -> b -> c
flipl = \f a b -> f b a

-- Aufgabe 4
--a)
{-
f.f
g.f
f.h
i.g
g.h
-}

--b)
{-
f.fst.i
f.snd.i
-}
