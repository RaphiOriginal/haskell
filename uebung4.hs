--Aufgabe 1
--a)
compareIf :: (Ord a) => a -> a -> Ordering
compareIf x y = if x < y then LT else 
    if x == y then EQ else GT
--b)
compareGuards :: (Ord a) => a -> a -> Ordering
compareGuards x y | x < y = LT
              | x == y = EQ
              | otherwise = GT
--c)
compareCase :: (Ord a) => a -> a -> Ordering
compareCase x y = if x < y then LT else
    case x == y of
        True -> EQ
        otherwise -> GT
--d)
{- 
Im patternmatching können keine werte verglichen werden um so eine verzweigung zu simulieren
-}

--Aufgabe 2
--a)
test :: [a] -> a
test (x:xs) = x
{- 
compiler compiliert ohne fehler, bei der Eingabe von text []
taucht dann aber folgende Exception auf:
*** Exception: uebung4.hs:21:1-15: Non-exhaustive patterns in function test
-}
--b)
definition1 :: (Int, Int, Int) -> (Int, Int, Int)
definition1 (_, b, c) = (b + c, b, c)
definition1 (a, b, _) = (a, b, a + b)
{-
Die Module laden korrekt aber der compiler gibt eine overlapped warnung raus:
1 of 1] Compiling Main             ( uebung4.hs, interpreted )

uebung4.hs:32:1: Warning:
    Pattern match(es) are overlapped
    In an equation for ‘definition1’: definition1 (a, b, _) = ...
Ok, modules loaded: Main.
-}

--Aufgabe 3
type M22 = ((Int, Int), (Int, Int))

--add
add :: M22 -> M22 -> M22
add (a1, b1) (a2, b2) = ((addTouples a1 a2), (addTouples b1 b2))

addTouples :: (Int, Int) -> (Int, Int) -> (Int, Int)
addTouples (x, y) (a, b) = (x + a, y + b)

--sub
sub :: M22 -> M22 -> M22
sub (a1, b1) (a2, b2) = ((subTouples a1 a2), (subTouples b1 b2))

subTouples :: (Int, Int) -> (Int, Int) -> (Int, Int)
subTouples (x, y) (a, b) = (x - a, y - b)

--mulS
mulS :: M22 -> Int -> M22
mulS (a, b) s = ((mulSTouple a s), (mulSTouple b s))

mulSTouple :: (Int, Int) -> Int -> (Int, Int)
mulSTouple (a, b) s = (a * s, b * s)

--mul
mul :: M22 -> M22 -> M22
mul ((x1, y1), (a1, b1)) ((x2, y2),(a2, b2)) = ((x1 * x2 + y1 * a2, x1 * y2 + y1 * b2), (a1 * x2 + b1 * a2, a1 *  y2 +  b1 * b2))

testTouple1 = ((1,1),(1,1)) :: M22
testTouple2 = ((2,2),(3,3)) :: M22