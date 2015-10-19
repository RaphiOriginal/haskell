{-# LANGUAGE NoMonomorphismRestriction #-}
import System.Environment (withArgs)
import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

-- Don't forget to add a type signature (:: Diagram B) to your creation
main = withArgs (words "-o diagram.svg -w 400") (defaultMain diag)
  where diag = (kunst 10) :: Diagram B

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

-- d)
