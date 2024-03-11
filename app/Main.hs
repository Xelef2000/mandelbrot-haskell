module Main where
import Graphics.Gloss
import Data.Complex


func :: Complex Float -> Complex Float -> Complex Float
func 0 _ = 0
func z c = (func (z - 1) c) ** 2 + c


mSequence :: Complex Float -> [Complex Float]
mSequence c = iterate (\z -> z ** 2 + c) 0


inSet :: Int -> Complex Float -> Bool
inSet n c = all (\z -> magnitude z <= 2) (take n (mSequence c))


generateImage :: Float -> Float -> Float -> Float -> Picture
generateImage xRes yRes xMin xMax = pictures [color (if inSet 10 (x :+ y) then black else white) (translate x y (rectangleSolid 10 10)) | x <- [xMin, xMin + xRes .. xMax], y <- [yMin, yMin + yRes .. yMax]]
  where
    yMin = -yMax
    yMax = -xMin


main :: IO ()
main = display (InWindow "Mandelbrot" (800, 800) (0, 0)) white (generateImage 0.005 0.005 (-2) 2)
