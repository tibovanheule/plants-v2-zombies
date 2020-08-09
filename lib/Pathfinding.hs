module Pathfinding where
import qualified Data.PQueue.Prio.Min as PQ
import qualified Data.HashSet as Set
import qualified Data.HashMap.Strict as Map
import Data.List (foldl')
import Data.Maybe (fromJust)
import Types

type Cost = Int

walls :: [(Coordinate,Coordinate)]
walls = [((1,7),(2,7)),((2,8),(2,7)),((0,7),(1,7)),((0,0),(1,0))]

defaultNeigbours :: Coordinate -> [Coordinate]
defaultNeigbours c = map (add c) [(1,0),(0,1),(-1,0),(0,-1)]
 where add (x,y) (x',y') = (x+x',y+y')

-- | Nowall checks if there is a wall between two coordinates.
nowall :: Coordinate -> Coordinate -> Bool
nowall current next = not ((current,next) `elem` walls || (next,current) `elem` walls)

astarSearch :: Coordinate -> (Coordinate -> Bool) -> (Coordinate -> [(Coordinate, Int)]) -> (Coordinate -> Int) -> Maybe (Int, [Coordinate])
astarSearch startNode isGoalNode nextNodeFn heuristic =
  astar (PQ.singleton (heuristic startNode) (startNode, 0))
         Set.empty (Map.singleton startNode 0) Map.empty
  where
    astar pq seen gscore tracks
      -- failed => return nothing
      | PQ.null pq = Nothing
      -- Success
      | isGoalNode node = Just (gcost, findPath tracks node)
      -- Already seen
      | Set.member node seen = astar pq' seen gscore tracks
      -- Else expand the node and continue
      | otherwise = astar pq'' seen' gscore' tracks'
      where
        -- Node with Least cost
        (node, gcost) = snd $ PQ.findMin pq
        -- Delete the node from open set
        pq' = PQ.deleteMin pq
        -- Add the node to the closed set
        seen' =  Set.insert node seen
        -- Find the successors (with their g and h costs) of the node
        -- which have not been seen yet
        successors =
          filter (\(s, g, _) ->
                    not (Set.member s seen') &&
                      (not (s `Map.member` gscore)
                        || g < (fromJust . Map.lookup s $ gscore)))
          $ successorsAndCosts node gcost

        -- Insert the successors in the open set
        pq'' = foldl' (\q (s, g, h) -> PQ.insert (g + h) (s, g) q) pq' successors

        gscore' = foldl' (\m (s, g, _) -> Map.insert s g m) gscore successors

        -- Insert the tracks of the successors
        tracks' = foldl' (\m (s, _, _) -> Map.insert s node m) tracks successors

    -- Finds the successors of a given node and their costs
    successorsAndCosts node gcost =
      map (\(s, g) -> (s, gcost + g, heuristic s)) . nextNodeFn $ node

    -- Constructs the path from the tracks and last node
    findPath tracks node | Map.member node tracks = findPath tracks (fromJust . Map.lookup node $ tracks) ++ [node]
                         | otherwise = [node]