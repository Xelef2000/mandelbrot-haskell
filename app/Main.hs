module Main where
import Graphics.Gloss
import Data.Complex
import Graphics.Gloss.Data.ViewPort

data Cell = Cell {x :: Float, y :: Float, value :: Bool}
  deriving (Show)


func :: Complex Float -> Complex Float -> Complex Float
func 0 _ = 0
func z c = (func (z - 1) c) ** 2 + c


mSequence :: Complex Float -> [Complex Float]
mSequence c = iterate (\z -> z ** 2 + c) 0


inSet :: Int -> Complex Float -> Bool
inSet n c
  | n == 0 = True
  | realPart c > 2 = False
  | imagPart c > 2 = False
  | otherwise = all (\z -> magnitude z <= 2) (take n (mSequence c))

decisionLimit :: Int
decisionLimit = 100

pixelSize :: Float
pixelSize = 10

windowSize :: (Int, Int)
windowSize = (400, 400)


-- (x,yPos, inSet decisionLimit (x :+ yPos))
--ypos xleft xright xres
generateRow :: Float -> Float -> Float -> Float -> [Cell]
generateRow yPos xMin xMax xRes = [ Cell x yPos (inSet decisionLimit (x :+ yPos)) | x <- [xMin, xMin + xRes .. xMax]]

-- Frame
-- xleft xright ybottom ytop, xres yres
generateFrame :: Float -> Float -> Float -> Float -> Float -> Float -> [[Cell]]
generateFrame xMin xMax yMin yMax xRes yRes = [generateRow y xMin xMax xRes | y <- [yMin, yMin + yRes .. yMax]]


displayCell :: Float -> Cell -> Picture
displayCell s (Cell x y value) = translate (unscale x) (unscale y) (color (if value then black else white) (rectangleSolid (s*pixelSize) (s*pixelSize)))
  where unscale x = x * 200

displayRow :: Float -> [Cell] -> Picture
displayRow s row = pictures $ map (displayCell s) row

displayFrame ::Float -> [[Cell]] -> Picture
displayFrame s frame = pictures $ map (displayRow s) frame


getVisibleCoordinates :: ViewPort -> (Float, Float, Float, Float)
getVisibleCoordinates viewport = (minX, maxX, minY, maxY)
  where
    (translateX, translateY) = viewPortTranslate viewport
    (imageWidth, imageHeight) = (fromIntegral $ fst windowSize, fromIntegral $ snd windowSize)
    lscale = viewPortScale viewport

    minX = -translateX
    minY = -translateY
    maxX = imageWidth * (1 / lscale)  - translateX
    maxY = imageHeight * (1 / lscale) - translateY




generateNewFrame :: ViewPort -> Float -> Picture -> Picture
generateNewFrame vp _ _ = displayFrame s $ generateFrame (lscale minX) (lscale maxX) (lscale minY) (lscale maxY) 0.05 0.05
  where
    (minX, maxX, minY, maxY) = getVisibleCoordinates vp
    s = viewPortScale vp
    lscale x = x / 200


initTest :: Picture
initTest = pictures [Text "Hello", Text "World"]

-- show the viewport
transitionTest :: ViewPort -> Float -> Picture -> Picture
transitionTest vp _ _ =   Scale 0.15 0.15 $ Text $ show (minX, maxX, minY, maxY)
  where (minX, maxX, minY, maxY) = getVisibleCoordinates vp

main :: IO ()
-- main = simulate (InWindow "Mandelbrot" (800, 800) (0, 0)) white 1 initTest id transitionTest

main = simulate (InWindow "Mandelbrot" windowSize (0, 0)) white 1 (displayFrame 1 (generateFrame (-2) 2 (-2) 2 0.05 0.05)) id generateNewFrame
-- main = display (InWindow "Mandelbrot" (800, 800) (0, 0)) white (displayFrame (generateFrame (-2) 2 (-2) 2 0.01 0.01))
-- main = do
--   print $ generateFrame (-2) 2 (-2) 2 0.5 0.5