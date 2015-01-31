-- game.hs
-- Bacteria colony simulator

-- Nicolas Hahn
-- Ashley Rocha
-- Artem Skhorokhodov

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Game
import Math.Geometry.Grid
import Math.Geometry.Grid.Square


gridSize :: Int
gridSize = 80

winSize = 800

cellSize :: Float
cellSize = 10

board = rectSquareGrid gridSize gridSize


playerCoords :: [(Float, Float)]
playerCoords = [(1,1),(1,0),(0,1),(2,1)]

enemyCoords :: [(Float, Float)]
enemyCoords = [(5,5),(5,6),(6,5),(6,6)]

-- Make coordinates correct bc gloss sets origin at center
getX :: (Float, Float) -> Float
getX (x,_) = (cellSize * x) - ((fromIntegral winSize) / 2) + (cellSize / 2)

getY :: (Float, Float) -> Float
getY (_,y) = (cellSize * y) - ((fromIntegral winSize) / 2) + (cellSize / 2)



tile :: Color -> Float -> Float -> Picture
tile c x y 
	= Translate (x) (y)
	$ Scale 1 1
	$ Color c
	$ Polygon (rectanglePath cellSize cellSize)


--takes function that makes one tile Picture from color, coordinates,
--and makes list of tile Pictures from color, list of coordinates
coordsToPics :: (Color -> Float -> Float -> Picture) -> Color -> [(Float, Float)] -> [Picture]
coordsToPics _ _ [] = []
coordsToPics f c (t:ts) = (f c (getX t) (getY t)): coordsToPics f c ts

allBacteria = 	Pictures ((coordsToPics tile green playerCoords )
				++ (coordsToPics tile red enemyCoords ))


main =
	display
        (InWindow
	       "Bacteria" 	 -- window title
		(winSize, winSize) 	 -- window size
		(400, 150)) 	 -- window position
		black			 -- background color
		allBacteria			 -- picture to display
