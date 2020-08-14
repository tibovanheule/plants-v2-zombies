module Constants where
import Types

directions :: [Coordinate]
directions = [(0.0,-1.0),(1.0,0.0),(0.0,1.0),(-1.0,0.0)]

defaultNeigbours :: Coordinate -> [Coordinate]
defaultNeigbours c = map (add c) directions
 where add (x,y) (x',y') = (x+x',y+y')

schaal, breedte, hoogte, halfSchaal :: Float
schaal = 70
breedte = 9
hoogte = 7
halfSchaal = schaal / 2