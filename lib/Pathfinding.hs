module Pathfinding where
import qualified Data.PQueue.Prio.Min as PQ
import qualified Data.HashSet as Set
import qualified Data.HashMap.Strict as Map
import Data.List (foldl')
import Data.Maybe (fromJust)
import Types
import Constants
import Debug.Trace

type Cost = Int

-- | Nowall checks if there is a wall between two coordinates.
nowall ::  Coordinate -> [(Coordinate,Coordinate)] -> Coordinate -> Bool
nowall current walls next = not ((current,next) `elem` walls || (next,current) `elem` walls)

-- | Checks if there is plant on a tile
noplant :: Coordinate -> [Plant] -> Bool
noplant c =  not . any ( (==) c . plantpos)

-- | Checks if a given coordinate falls in the grid.
inbound :: Coordinate -> Bool
inbound (x,y) = x >= 0 && x < 9 && y < 6 && y >= 0

-- | Gives back an array of coordinates to use for the open list
possibleMoves :: [(Coordinate,Coordinate)] -> [Plant] -> Coordinate -> Coordinate -> [Coordinate]
possibleMoves walls plants end c = filter (\x -> nowall c walls x && inbound x && (noplant x plants || x == end)) (defaultNeigbours c)

-- | heuristic for the a-star search algorithm. Simplified version of euclidean distance
heuristic :: Coordinate -> Coordinate -> Int
heuristic (x,y) (x',y') = round $ abs(x'-x) + abs (y'-y)

-- | seek the shortest path from start to end, using A-star algo. The dependencies for this algorithm are the same as reactive-banana
astarSearch :: [(Coordinate,Coordinate)] -> [Plant] -> Coordinate -> Coordinate -> Maybe [Coordinate]
astarSearch walls plants startNode end = astar (PQ.singleton (heuristic end startNode) (startNode, 0)) Set.empty (Map.singleton startNode 0) Map.empty
 where
    isEnd c = c == end
    astar open seen gscore tracks
      -- failed => return nothing
      | PQ.null open = Nothing
      -- Success
      | isEnd node = Just ( getPath tracks node)
      -- Already seen
      | Set.member node seen = astar open' seen gscore tracks
      -- Else expand the node and continue
      | otherwise = astar open'' seen' gscore' tracks'
      where
        -- Node with Least cost
        (node, gcost) = snd $ PQ.findMin open
        -- Node already seen
        seen' =  Set.insert node seen
        -- Delete the node from open set
        open' = PQ.deleteMin open
        -- Conditions : - not in the seen set.
        --              - and not in in gscore map or with a lower score (shorter pad)
        cond (s,g,_) = not (s `Set.member` seen') && (not (s `Map.member` gscore) || g < (fromJust . Map.lookup s $ gscore))
        -- The successors, g and h costs of a node, filter acoordingly with the cond function above
        successors = filter cond $ successorsAndCosts gcost node
        -- Insert the successors in the open set
        open'' = foldl' (\q (s, g, h) -> PQ.insert (g + h) (s, g) q) open' successors

        gscore' = foldl' (\m (s, g, _) -> Map.insert s g m) gscore successors

        -- Insert the tracks of the successors
        tracks' = foldl' (\m (s, _, _) -> Map.insert s node m) tracks successors

    -- Successors of a node (posible moves) and their cost
    successorsAndCosts gcost = map (\s -> (s, gcost + 1, heuristic end s)) . possibleMoves walls plants end
    -- If a Path is possible, construct it
    getPath tracks node | Map.member node tracks = getPath tracks (fromJust . Map.lookup node $ tracks) ++ [node] -- nod is meber of tracks
                        | otherwise = [node]
