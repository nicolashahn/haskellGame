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
import Data.Char


gridSize :: Int
gridSize = 50

winSize = 750

cellSize :: Float
cellSize = 15

board = rectSquareGrid gridSize gridSize

-- the int is the population at the tile
playerCoords :: [(Float, Float, Int)]
playerCoords = [(1,1,5),(1,0,4),(0,1,6),(2,1,2)]

enemyCoords = [(49,49,3),(49,48,4),(48,49,3),(48,48,4)]

foodCoords = [(20,20,0), (38,10,0), (21,30,0)]

-- Make coordinates correct bc gloss sets origin at center
getX :: (Float, Float, Int) -> Float
getX (x,_,_) = (cellSize * x) - ((fromIntegral winSize) / 2) + (cellSize / 2)
getY (_,y,_) = (cellSize * y) - ((fromIntegral winSize) / 2) + (cellSize / 2)

-- get third element of tuple
thd :: (a, b, c) -> c
thd (_,_,c) = c


-- one square tile of given color at x, y
tile :: Color -> Float -> Float -> Picture
tile c x y 
	= Translate (x) (y)
	$ Scale 1 1
	$ Color c
	$ Polygon (rectanglePath cellSize cellSize)

-- print an int at x, y
tileNum :: Int -> Float -> Float -> Picture
tileNum i x y 
	= Translate (x-cellSize/5) (y-cellSize/4)
	$ Scale 0.08 0.08
	$ Color white
	$ Text (show i)


--takes function that makes one tile Picture with parameter(color), coordinates,
--and makes list of tile Pictures from color, list of coordinates
tiles :: (a -> Float -> Float -> Picture) -> a -> [(Float, Float, Int)] -> [Picture]
tiles _ _ [] = []
tiles f a (t:ts) = (f a (getX t) (getY t)): tiles f a ts

-- same as above but for tile population
tileNums :: (Int -> Float -> Float -> Picture) -> [(Float, Float, Int)] -> [Picture]
tileNums _ [] = []
tileNums f (t:ts) = (f (thd t) (getX t) (getY t)): tileNums f ts

allPics = 	Pictures ((tiles tile blue playerCoords)
					++ (tiles tile red enemyCoords)
					++ (tiles tile green foodCoords)
					++ (tileNums tileNum playerCoords)
					++ (tileNums tileNum enemyCoords)
					)


main =
	display
        (InWindow
	       "Bacteria" 	 -- window title
		(winSize, winSize) 	 -- window size
		(400, 150)) 	 -- window position
		black			 -- background color
		allPics			 -- picture to display
