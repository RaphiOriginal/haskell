module PictureASCII where
import Data.Char

--------------------------------------------------------------------------------
-- 1) ASCII Pictures
--------------------------------------------------------------------------------

-- A Picture is a list of strings.
type Picture = [String]

-- An example picture. Design a nicer one.
lambda :: Picture
lambda = [
 "                   ",
 "     ####          ", 
 "       ###         ",
 "        ###        ",
 "        ####       ",
 "       ## ###      ",
 "      ##   ###     ",
 "    ###     ###    ",
 "                   "]

rock :: Picture
rock = [
 "                   ",
 "   ###########     ",
 "     ##      ###   ",
 "     ##      ###   ",
 "     #########     ",
 "     ##     ###    ",
 "     ##      ###   ",
 "   ####       ###  ",
 "                   "]

-- Run 'main' to see an example.
main :: IO ()
main = do printPicture lambda

-- Prints the given picture to stdout.
printPicture :: Picture -> IO ()
printPicture p = putStr (unlines p)

-- a) Mirrors the given picture horizontally.                    
flipH :: Picture -> Picture
flipH e = reverse e

-- b) Mirrors the given picture vertically.
-- hint = "Wrqr Mrvyr zhff frcneng hztrxrueg jreqra."
flipV :: Picture -> Picture
flipV [] = []
flipV (x:xs) = [reverse x] ++ (flipV xs)


-- c) Takes two pictures and puts the first above the second.
-- hint = "Qnf fbyygra Fvr nhpu buar Uvysr uvaxevrtra."
above :: Picture -> Picture -> Picture
above a b = a ++ b

-- d) Takes two pictures and puts the first left of the second.
-- hint = "Irejraqra Fvr qvr Shaxgvba mvcJvgu hz wr mjrv Mrvyra mh xbaxngravrera."
beside :: Picture -> Picture -> Picture
beside [] [] = []
beside (x:xs) (y:ys) = [(x ++ y)] ++ (beside xs ys)

--------------------------------------------------------------------------------
-- 2) Functions on Lists
-- hint = "\65533oreyrtra Fvr fvpu mhrefg qvr Fvtanghe"
--------------------------------------------------------------------------------
-- a) Append an element at the end.
append :: [a] -> a -> [a]
append a b = a ++ [b]

-- b) Reverse all but the first and the last element.
reverse2 :: [a] -> [a]
reverse2 (x:xs) =  [x] ++ (reverse (init xs)) ++ [(last xs)]

-- c) Insert an element at a given position.
put :: Int -> [a] -> a -> [a]
put n a b = (take n a) ++ [b] ++ (drop n a)


-- Functions to decode hints.
-- Usage: decodeHint "Mhrefg anpuqraxra, qnaa Gvccf nafpunhra =)"
decodeHint :: String -> String
decodeHint = map rot13
  where rot13 c
         | toLower c >= 'a' && toLower c <= 'm' = chr (ord c + 13)
         | toLower c >= 'n' && toLower c <= 'z' = chr (ord c - 13)
         | otherwise = c




