-- game.hs

-- Nicolas Hahn
-- Ashley Rocha
-- Artem Skhorokhodov


-- | Display "Hello World" in a window.
--
import Graphics.Gloss



main 	
 = display 
        (InWindow
	       "Bacteria" 	 -- window title
		(800, 600) 	 -- window size
		(400, 200)) 	 -- window position
	black			 -- background color
	picture			 -- picture to display

picture	
	= Translate (0) (0) -- shift the text to the middle of the window
	$ Scale 1 1
	$ Color white
	$ Polygon (rectanglePath 5 5)
