-- grid.hs
-- prints grid to display

import Graphics.Gloss
import Math.Geometry.Grid
import Math.Geometry.Grid.Square

gLength = 25 -- length of grid
gSize = gLength * gLength
grid = rectSquareGrid gLength gLength
gCoors = indices grid

-- converts Grid coordinates to pixel coordinates for printing purposes
coorsToPixels coors = map (\(x, y) -> (convert x, convert y)) coors
    where convert a = fromIntegral(a * gLength)

-- list of pixel coordinates
gPixelCoors = coorsToPixels gCoors

-- take in a list of pixel coordinates for a grid and creates a list of alternating colored squares
gPicture coors = map (\(x, y) -> translate (adj x) (adj y) $ color (col (adj x)  (adj y)) $ rectangleSolid (fromIntegral (gLength)) (fromIntegral(gLength))) coors
    where adj a = fromIntegral(a - (gSize `div` 2 - gLength `div` 2)) -- adjust print coordinates to account for center origin; convert to float
          isEven n = ceiling n `div` gLength `mod` 2 == 0
          col x y = if (isEven x && isEven y) || ((not (isEven x)) && not (isEven y)) then blue else cyan

-- convert list of pictures to single picture
printPic = pictures (gPicture gPixelCoors)

main 	
 = display 
        (InWindow
	     "Grid" 	 -- window title
		(gSize, gSize) 	 -- window size
		(0, 0)) 	 -- window positioned in center
	white			 -- background color
    printPic
    -- picture to display

