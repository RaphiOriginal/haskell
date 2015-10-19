{-# LANGUAGE NoMonomorphismRestriction #-}
import System.Environment (withArgs)
import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

-- Don't forget to add a type signature (:: Diagram B) to your creation
main = withArgs (words "-o diagram.svg -w 400") (defaultMain diag)
  where diag = (fancyShit 20) :: Diagram B

atopExample, besideExample, aboveExample :: Diagram B
atopExample   = fc black (square 1) `atop` circle 1
besideExample = fc black (square 1) ||| circle 1
aboveExample  = fc black (square 1) === circle 1

-- Aufgabe 3
-- a)
recTriangle :: Int -> Diagram B
recTriangle n = (mempty (fromIntegral n)) `atop` newTriangle ((fromIntegral n) - 1)
newTriangle :: Double -> Diagram B
newTriangle 1 = eqTriangle 1
newTriangle n = (eqTriangle n) `atop` (newTriangle (n - 1))

-- b)
darkCircle :: Int -> Diagram B
darkCircle 1 = fc white (circle (fromIntegral 1))
darkCircle n = case odd n of
                        True -> darkCircle (n-1) `atop` (fc white (circle (fromIntegral n)))
                        False -> darkCircle (n-1) `atop` (fc black (circle (fromIntegral n))) 

-- c)
kunst :: Int -> Diagram B
kunst n = kunst' n 0
kunst' :: Int -> Double -> Diagram B
kunst' 1 d = rotateBy (d / 6) (fc white (square (fromIntegral 1)))
kunst' n d = case odd n of
                        True -> kunst' (n-1) (d + 1) `atop` (rotateBy (d / 6) (fc white (square (fromIntegral n))))
                        False -> kunst' (n-1) (d + 1) `atop` (rotateBy (d / 6) (fc black (square (fromIntegral n))))

-- d) (optional)
fancyShit :: Int -> Diagram B
fancyShit n = fancyShit' n

fancyShit' :: Int -> Diagram B
fancyShit' 3 = fc beige (regPoly 3 3  # lw none)
fancyShit' n = case mod n 11 of
    0 -> fancyShit' (n - 1) `atop` (fc aqua (regPoly (fromIntegral n) (fromIntegral n) # lw none))
    1 -> fancyShit' (n - 1) `atop` (fc aquamarine (regPoly (fromIntegral n) (fromIntegral n) # lw none))
    2 -> fancyShit' (n - 1) `atop` (fc azure (regPoly (fromIntegral n) (fromIntegral n) # lw none))
    3 -> fancyShit' (n - 1) `atop` (fc beige (regPoly (fromIntegral n) (fromIntegral n) # lw none))
    4 -> fancyShit' (n - 1) `atop` (fc bisque (regPoly (fromIntegral n) (fromIntegral n) # lw none))
    5 -> fancyShit' (n - 1) `atop` (fc blue (regPoly (fromIntegral n) (fromIntegral n) # lw none))
    6 -> fancyShit' (n - 1) `atop` (fc blueviolet (regPoly (fromIntegral n) (fromIntegral n) # lw none))
    7 -> fancyShit' (n - 1) `atop` (fc cadetblue (regPoly (fromIntegral n) (fromIntegral n) # lw none))
    8 -> fancyShit' (n - 1) `atop` (fc chartreuse (regPoly (fromIntegral n) (fromIntegral n) # lw none))
    9 -> fancyShit' (n - 1) `atop` (fc coral (regPoly (fromIntegral n) (fromIntegral n) # lw none))
    10 -> fancyShit' (n - 1) `atop` (fc darkcyan (regPoly (fromIntegral n) (fromIntegral n) # lw none))