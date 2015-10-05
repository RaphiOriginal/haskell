--a)
maxGuards :: Int -> Int -> Int
maxGuards a b | a > b = a
              | otherwise = b

--b)
maxIf :: Int -> Int -> Int
maxIf a b = if a < b then b else a

--c)
maxCase :: Int -> Int -> Int
maxCase a b = case a < b of
    False     -> a
    otherwise -> b

--d)
{-
Nein, da wir keine boolean werte Matchen können (zumindest nicht welche,
die vorher berechnet werden müssen)
-}