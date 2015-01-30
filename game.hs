-- game.hs

-- Nicolas Hahn
-- Ashley Rocha
-- Artem Skhorokhodov


-- | Display "Hello World" in a window.
--
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Math.Geometry.Grid
import Math.Geometry.Grid.Square

winSize :: Int
winSize = 500

board = rectSquareGrid 50 50

cellSize :: Float
cellSize = 10

bacteria :: [(Float, Float)]
bacteria = [(0,0),(1,0),(0,1),(25,25)]

-- Make coordinates correct for gloss display
getX :: (Float, Float) -> Float
getX (x,_) = (cellSize * x) - 245
--getX (x,_) = (cellSize * x) - ((fromIntegral winSize) / 2) - div cellSize 2)

getY :: (Float, Float) -> Float
getY (_,y) = (cellSize * y) - 245
--getY (_,y) = (cellSize * y) - ((fromIntegral winSize) / 2) - div cellSize 2)


oneBacteria :: Float -> Float -> Picture
oneBacteria x y
	= Translate (x) (y)
	$ Scale 1 1
	$ Color white
	$ Polygon (rectanglePath cellSize cellSize)


--take list of bacteria coordinates and make list of pictures out of them
makeBacteria :: [(Float, Float)] -> [Picture]
makeBacteria [] = []
makeBacteria (c:cs) = (oneBacteria (getX c) (getY c)): makeBacteria cs

allBacteria = Pictures (makeBacteria bacteria)


main =
	display
        (InWindow
	       "Bacteria" 	 -- window title
		(winSize, winSize) 	 -- window size
		(400, 200)) 	 -- window position
		black			 -- background color
		allBacteria			 -- picture to display
