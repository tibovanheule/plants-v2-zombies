-- | Implementation of the game logic.
module Game where
import           Types
import Debug.Trace
import Data.Maybe (isJust)
changeWorld :: World -> World
changeWorld (World time (Just l@(Level _ _ _ z p phases en _)) _ levels currlevel) = World (time+1) level newState levels currlevel
                                         where zom =  map (actZombie p) (dead z ++ spawn time phases)
                                               filteredPhases = filterPhases time phases
                                               plant = movePeas z $ map shoot p
                                               currentState = head filteredPhases
                                               newState = isWon time currentState z
                                               level = Just (l { zombies=zom, phase= filteredPhases, plants=plant, energy=calcEnergy en p})
--- GAME LOGIC

-- | add a plant to a level
addplant :: Level -> Coordinate -> Level
addplant l c@(x,y) | energy l - cost >= 0  && isJust (chosenplant l) &&  isCoorTaken =  l { plants=plant ++ plants l, energy = energy l - cost}
                   | otherwise = l
 where isCoorTaken = not $ any check (plants l)
       check (Plant _ _ (x',y') _  _ _) = (x == x') && (y == y')
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
dead = filter (\(Zombie _ l _ _ _) -> l > 0)

-- | move a zombie , figth with plant, or get hit only if possible.
actZombie :: [Plant] -> Zombie -> Zombie
actZombie plants z@(Zombie _ life c@(x,y) _ speed) = z { zombiepos=pos, zombielife=newlife }
               where isHit (Pea (x',y') _ _ _) = abs(x - x') <= 0.5 && abs(y - y') <= 0.5
                     peas = filter isHit $ concatMap shots plants
                     newlife | null peas =  life
                             | otherwise = sum $ map ((life -) . peadamage) peas
                     pos = case zombieBeforePlant plants (x-1.0,y) of
                                [] -> (x-(speed/60),y)
                                p -> c


-- | Checks if there is a plant that stands in the way of the zombie
zombieBeforePlant :: [Plant] -> Coordinate -> [Plant]
zombieBeforePlant p (x,y) = filter check p
                            where check (Plant _ _ (x',y') _ _ _) = (x' - x) > 0 && (x' - x) < 1 && y' == y

movePeas :: [Zombie] -> [Plant] -> [Plant]
movePeas z = map (\p -> p {shots = newshots p} )
 where outOfBound (Pea (x,y) _ _ _) = y > 5 || x < 0 || x > 8 || y < 0
       delPeaCond p = outOfBound p || any (isHit p) z
       isHit (Pea (x,y) _ _ _) (Zombie _ _ (x',y') _ _) = abs(x - x') <= 0.5 && abs(y - y') <= 0.5
       movePea  p@(Pea (x,y) _ _ (x',y')) = p {peapos = (x+x',y+y')}
       newshots = map movePea . filter ( not . delPeaCond) . shots

-- | geeft de staat van spel terug
isWon :: Time -> Phases -> [Zombie] -> State
isWon t p@(Phases dur EndPhase _) [] | t >= (dur * 60) = Won
                                     | otherwise = Ongoing
isWon t p@(Phases dur EndPhase _) (_:_) | t >= (dur * 60) = Lost
                                        | otherwise = Ongoing
isWon _ _ z = Ongoing

zombiePosCheck :: [Zombie] -> State
zombiePosCheck zombies | any (\z -> (fst $ zombiepos z) <= 0) zombies = Lost
                       | otherwise = Ongoing

-- | calculates total amount of energy, uses checkSunflower function ()
calcEnergy :: Energy -> [Plant] -> Energy
calcEnergy en p = en + sum (map checkSunflower p)


-- | check if 3 seconds has passed, and add energy if needed
checkSunflower :: Plant -> Energy
checkSunflower (Plant Sunflower _ _ 180 _ _) = 1
checkSunflower _ = 0

shoot :: Plant -> Plant
shoot p@(Plant Sunflower _ _ t _ _ ) | t > 180 = p {lastshot=1}
                                     | otherwise = p {lastshot=lastshot p + 1}
shoot p@(Plant Peashooter _ _ t _ _ ) | t == 120 = p {lastshot=0,shots=peas (plantpos p) ++ shots p}
                                      | otherwise =  p {lastshot=lastshot p + 1}
shoot p = p

peas :: Coordinate -> [Pea]
peas c = map (createPea c) [left,right,up,down]
                                 
-- | Get all spawns of zombies out of a phase
spawn :: Time -> [Phases] -> [Zombie]
spawn t = concatMap (createZombies t) . getCurrentPhaseSpawns

-- | een lijst van zombies maken als dit nodig blijkt
createZombies :: Time -> Spawn -> [Zombie]
createZombies time (Spawn r l zombies) | any ((==) time . (*60) ) r = concatMap putZombieOnLane l
                                       | otherwise = []
 where putZombieOnLane lane = map (\z -> z {zombiepos=(fst $ zombiepos z,lane-1)} ) zombies

-- | use Time to filter Phases that ended
filterPhases :: Time -> [Phases] -> [Phases]
filterPhases t p | length p > 1 = del (p !! 1)
                 | otherwise = p
 where
  del (Phases start _ _) | (start * 60) < t = tail p
                         | otherwise = p

getCurrentPhaseSpawns :: [Phases] -> [Spawn]
getCurrentPhaseSpawns [] = []
getCurrentPhaseSpawns h = phaseSpawns $ head h



  -- round10 x = (fromInteger $ round $ x *10) / 10