-- game.hs

-- Nicolas Hahn
-- Ashley Rocha
-- Artem Skhorokhodov


-- | Display "Hello World" in a window.
--
import Graphics.Gloss
import GHC.Float
import Math.Geometry.Grid

cellSize :: Int
cellSize = 5

cellFloat :: Float
cellFloat = int2Float cellSize

gridWidth :: Int
gridWidth = cellSize * 150

gridHeight :: Int
gridHeight = cellSize * 100.0

board = rectSquareGrid 50 50


main 	
 = display 
        (InWindow
	       "Bacteria" 	 -- window title
		(gridWidth, gridHeight) 	 -- window size
		(400, 200)) 	 -- window position
	black			 -- background color
	picture			 -- picture to display

picture	
	= Translate (0) (0) -- shift the text to the middle of the window
	$ Scale 1 1
	$ Color white
	$ Polygon (rectanglePath cellFloat cellFloat)
