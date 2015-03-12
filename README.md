# haskellGame
Bacterial colony growth game using Haskell + Gloss
By Nicolas Hahn, Ashley Rocha

Mechanics:
- Two bacterial colonies
  - Each colony starts with one cell, population 1
  - Population is how many bacteria cells are on that grid position
  - Take turns growing and fighting
- Growth algorithm
  - If population hits certain number, cells break off to an adjacent position
  - Random in simulation version
  - Directed in game version - grows toward a certain position in grid
- Combat algorithm
  - When two bacteria of opposing sides meet, one or both dies based on how large their population is


sim.hs - Just the simulator version, no user input. Starts off with two single cells that grow randomly and fight until one is completely wiped off the map.

main.hs - Keyboard input added, control the blue dot and the green colony will 'follow' it (in the growth stage of each turn, it chooses the available position closest to the blue dot, instead of randomly like in the sim version). A cap on how many cells each colony can have has also been added, to allow for finer control of a colony. The red colony will choose to grow towards the average position of the green colony (middle of the clump of cells).


See both versions in action here: https://www.youtube.com/watch?v=mLZGIqlDHc8


Still to do:
- Food pellets
  - randomly spawn on the grid after certain intervals of time
  - when a colony grows a cell on top of one, it could temporarily increase the max cell cap, or increase population growth, or something else
  - Poison pellets that do something negative?
- Better enemy AI
  - right now it just grows towards you, pretty dumb
  - make it grow towards food pellets if they appear?
  - try to wrap around player colony
- Obstacles on map
  - walls, islands, moving blockades, something to make the grid less boring
- Better controls
  - ideally if we're trying to get the colony to grow towards a certain grid position, mouse controls would be more intuitive and give the user better control
