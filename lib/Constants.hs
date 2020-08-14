module Constants where
import Types

-- | All directions of coordinate
directions :: [Coordinate]
directions = [(0.0,-1.0),(1.0,0.0),(0.0,1.0),(-1.0,0.0)]

-- | genetate all possible Neigbours for a given coordinate
defaultNeigbours :: Coordinate -> [Coordinate]
defaultNeigbours c = map (add c) directions
 where add (x,y) (x',y') = (x+x',y+y')

-- | schaal, width, heigth for gloss window
schaal, breedte, hoogte, halfSchaal :: Float
schaal = 70
breedte = 9
hoogte = 7
halfSchaal = schaal / 2

-- | function to generate all possible grid locations
gridCoors :: [Coordinate]
gridCoors = concatMap (flip zip [0..5] . replicate 9) [0..8]