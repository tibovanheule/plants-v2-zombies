-- | Implementation of the game logic.
module Game where
import Types
import Data.Maybe (isJust)
import Data.List
import Data.Function
import Pathfinding

changeWorld :: World -> World
changeWorld w@(World time (Just l@(Level _ _ _ z p phases en _ levelmap)) _ _ _) = w { worldtime = 1 + worldtime w, chosenLevel= Just newlevel, state=newState}
                                         where zom =  map (actZombie (wall levelmap) (homes levelmap) p) (dead z ++ spawn time phases)
                                               filteredPhases = filterPhases time phases
                                               plant = movePeas z $ map (shoot z) p
                                               currentState = head filteredPhases
                                               newState = isWon time currentState z (homes levelmap)
                                               newlevel = l { zombies=zom, phase= filteredPhases, plants=plant, energy=calcEnergy en p}

-- | add a plant to a level
addplant :: Level -> Coordinate -> Level
addplant l c | energy l - cost >= 0  && isJust (chosenplant l) &&  isCoorTaken =  l { plants=plant ++ plants l, energy = energy l - cost}
             | otherwise = l
 where isCoorTaken = not $ any ((==) c . plantpos) (plants l)
       cost = case chosenplant l of
                   Just Walnut -> 6
                   Just Peashooter -> 6
                   Just Sunflower -> 3
                   Nothing -> 0
       plant = case chosenplant l of
                   Just Walnut -> [createWalnut c]
                   Just Peashooter -> [createPeaShooter c]
                   Just Sunflower -> [createSunflower c]
                   Nothing -> []

-- | filter away dead zombies
dead :: [Zombie] -> [Zombie]
dead = filter ((<) 0 . zombielife)

-- | move a zombie , figth with plant, or get hit only if possible.
actZombie :: [(Coordinate,Coordinate)] -> [Coordinate] -> [Plant] -> Zombie -> Zombie
actZombie walls home plants z@(Zombie _ _ c@(x,y) _ speed _) = z { zombiepos=pos, zombielife=newlife, zombielastattack= newzombielastattack}
               where peas = filter (isHit c . peapos) $ concatMap shots plants
                     newlife | null peas =  zombielife z
                             | otherwise = sum $ map ((zombielife z -) . peadamage) peas
                     newzombielastattack | zombielastattack z == 3*60 = 0
                                         | otherwise = 1 + zombielastattack z
                     pos | any (isHit c . plantpos) plants = c
                         | otherwise = newCoorUsingPathfinding plants c walls home speed

-- | try to use pathfinding to get to get to destination or nearest plant
newCoorUsingPathfinding :: [Plant] -> Coordinate -> [Wall] -> [Coordinate] -> Float -> Coordinate
newCoorUsingPathfinding plants c@(x,y) walls homes speed  =
  case astarSearch walls plants (fromIntegral $ round x,fromIntegral $ round y) (head homes) of
                                                          Just t -> newpos' t
                                                          Nothing -> nearestPlantAttack
  where newpos' t | length t > 1 = newpos c $ t !! 1
                  | length t == 1 = newpos c $ head t
                  | otherwise = c
        -- Calculate new coordinate for zombie
        newpos (x',y') (x'',y'') = (x'+((x''-x')*speed/60),y'+((y''-y')*speed/60))
        -- Search for route to the nearest plant
        nearestPlantAttack = case astarSearch walls plants (fromIntegral $ round x,fromIntegral $ round y) nearestPlantCoor of
                                  Just t -> newpos' t -- path to the nearest plant found
                                  Nothing -> c -- Give up! no path to nearest plant or route to home, probably an impossible maze (home surrounded by walls)
        -- Find the nearest Plant for a given zombie
        nearestPlantCoor = plantpos $ minimumBy (compare `on` (euclidean c . plantpos) ) plants
        -- The distance function is simplified. sqrt(x) < sqrt(y) <=> x < y
        euclidean (x,y) (x',y') = abs(x'-x) + abs (y'-y)

movePeas :: [Zombie] -> [Plant] -> [Plant]
movePeas z = map (\p -> p {shots = newshots p} )
 where outOfBound (Pea (x,y) _ _ _) = y > 5 || x < 0 || x > 8 || y < 0
       delPeaCond p = outOfBound p || any (isHit (peapos p) . zombiepos) z
       movePea  p@(Pea (x,y) _ _ (x',y')) = p {peapos = (x+x',y+y')}
       newshots = map movePea . filter ( not . delPeaCond) . shots

-- | Determines if two coordinates are close together
isHit :: Coordinate -> Coordinate -> Bool
isHit (x,y) (x',y') = abs(x - x') <= 0.5 && abs(y - y') <= 0.5

-- | geeft de staat van spel terug
isWon :: Time -> Phases -> [Zombie] -> [Coordinate] -> State
isWon t p@(Phases dur EndPhase _) []  _ | t >= (dur * 60) = Won
                                        | otherwise = Ongoing
isWon t p@(Phases dur EndPhase _) (_:_) _ | t >= (dur * 60) = Lost
                                          | otherwise = Ongoing
isWon _ _ z h | any (zombiePosCheck z) h = Lost
              | otherwise = Ongoing

-- | check if there is any zombie on a home
zombiePosCheck :: [Zombie] -> Coordinate -> Bool
zombiePosCheck zombies (x,y) = any ( diff . zombiepos ) zombies
 where diff (x',y') = abs(x - x') <= 0.5 && abs(y - y') <= 0.5

-- | calculates total amount of energy, uses checkSunflower function ()
calcEnergy :: Energy -> [Plant] -> Energy
calcEnergy en p = en + sum (map checkSunflower p)


-- | check if 3 seconds has passed, and add energy if needed
checkSunflower :: Plant -> Energy
checkSunflower (Plant Sunflower _ _ 180 _ _) = 1
checkSunflower _ = 0

shoot :: [Zombie] -> Plant -> Plant
shoot _ p@(Plant Sunflower _ _ t _ _ ) | t > 3 * 60 = p {lastshot=1}
                                       | otherwise = p {lastshot=lastshot p + 1}
shoot z p@(Plant Peashooter _ _ t _ _ ) | t == 2 * 60 && null z = p {lastshot=0}
                                        | t == 2 * 60 = p {lastshot=0,shots=peas z (plantpos p) : shots p}
                                        | otherwise =  p {lastshot=lastshot p + 1}
shoot _ p = p

-- | Find closest zombie and ATTACKS, DIE ZOMBIE, DIE ZOMBIE
peas :: [Zombie] -> Coordinate -> Pea
peas z c = createPea c $ direction c zombie
  where direction (x,y) (x',y') = ((x'-x) /60,(y'-y)/60)
        zombie = zombiepos $ minimumBy (compare `on` (euclidian c . zombiepos) ) z
        -- The distance function is simplified. sqrt(x) < sqrt(y) => x < y
        euclidian (x,y) (x',y') = abs(x'-x) + abs (y'-y)
                                 
-- | Get all spawns of zombies out of a phase
spawn :: Time -> [Phases] -> [Zombie]
spawn t = concatMap (createZombies t) . getCurrentPhaseSpawns
 where getCurrentPhaseSpawns = phaseSpawns . head

-- | een lijst van zombies maken als dit nodig blijkt
createZombies :: Time -> Spawn -> [Zombie]
createZombies time (Spawn r g zombies) | any ((==) time . (*60)) r = concatMap putZombieOnGrave g
                                       | otherwise = []
 where putZombieOnGrave grave = map (\z -> z {zombiepos=grave} ) zombies

-- | use Time to filter Phases that ended
filterPhases :: Time -> [Phases] -> [Phases]
filterPhases t p | length p > 1 = del (p !! 1)
                 | otherwise = p
 where del (Phases start _ _) | (start * 60) < t = tail p
                              | otherwise = p

