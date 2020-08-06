-- | Implementation of the game logic.
module Game where
import           Types
import Debug.Trace
changeWorld :: World -> World
changeWorld (World time (Just l@(Level _ _ _ z p phases en)) _ levels currlevel) = World (time+1) level newState levels currlevel
                                         where zom =  (map (actZombie p) (dead z)) ++ spawn time phases
                                               filteredPhases = filterPhases time phases
                                               plant = movePeas $ shootPlants p
                                               currentState = head filteredPhases
                                               newState = isWon time currentState z
                                               level = Just (l { zombies=zom, phase= filteredPhases, plants=plant, energy=calcEnergy en p})
--- GAME LOGIC

-- | filter away dead zombies
dead :: [Zombie] -> [Zombie]
dead = filter isAlive
       where isAlive (Zombie _ l _ _ _) = l > 0

-- | move a zombie or figth with plant, only if possible.
actZombie :: [Plant] -> Zombie -> Zombie
actZombie plants z@(Zombie _ _ c@(x,y) _ speed) = z { zombiepos=pos }
               where pos = case zombieBeforePlant plants (x-1,y) of
                                [] ->  (x-(speed/60),y)
                                p -> c

isHit :: Coordinate -> Coordinate -> Bool
isHit (x,y) (x',y') = x - x' <= 10 && y - y' <= 10

-- | Checks if there is a plant that stands in the way of the zombie
zombieBeforePlant :: [Plant] -> Coordinate -> [Plant]
zombieBeforePlant p (x,y) = filter check p
                            where check (Plant _ _ (x',y') _ _) = (x' >= x && y' == y)

movePeas :: [Plant] -> [Plant]
movePeas = map (\p -> p {shots = newshots p} )
 where movePea  p@(Pea (x,y) _ _ (x',y')) = p {peapos = (x+x',y+y')}
       newshots = map movePea . getPeas




-- | Reduce a Life with damage x
damage :: Life -> Damage -> Life
damage = (-)

-- | geeft de staat van spel terug
isWon :: Time -> Phases -> [Zombie] -> State
isWon t p@(Phases dur EndPhase _) [] | t >= (dur * 60) = Won
                                     | otherwise = Ongoing
isWon t p@(Phases dur EndPhase _) (_:_) | t >= (dur * 60) = Lost
                                        | otherwise = Ongoing
isWon _ _ z = Ongoing

zombiePosCheck :: [Zombie] -> State
zombiePosCheck zombies | any (\z -> getXZombie z <= 0) zombies = Lost
                       | otherwise = Ongoing

-- | calculates total amount of energy, uses checkSunflower function ()
calcEnergy :: Energy -> [Plant] -> Energy
calcEnergy en p = en + (sum $ map checkSunflower p)


-- | check if 3 seconds has passed, and add energy if needed
checkSunflower :: Plant -> Energy
checkSunflower (Plant Sunflower _ _ lastshot _) | lastshot == 180 = 1
                                                | otherwise = 0
checkSunflower _ = 0

shootPlants :: [Plant] -> [Plant]
shootPlants = map shoot

shoot :: Plant -> Plant
shoot p@(Plant Sunflower _ _ t _) | t > 180 = p {lastshot=1}
                                         | otherwise = p {lastshot=lastshot(p)+1}
shoot p@(Plant Peashooter _ _ t _) | t >= 120 = p {lastshot=0,shots=peas (plantpos(p)) ++ shots(p)}
                                          | otherwise =  p {lastshot=lastshot(p)+1}
schoot p = p

peas :: Coordinate -> [Pea]
peas c = map (createPea c) [left,right,up,down]
                                 
-- | Get all spawns of zombies out of a phase
spawn :: Time -> [Phases] -> [Zombie]
spawn t = checkSpawns t . getCurrentPhaseSpawns

-- controleerd de spaws en een lijst van zombies maken als dit nodig blijkt
checkSpawns :: Time -> [Spawn] -> [Zombie]
checkSpawns t = concatMap (createZombies t)

-- een lijst van zombies maken als dit nodig blijkt
createZombies :: Time -> Spawn -> [Zombie]
createZombies t (Spawn r l z) | needed t r = putZombieOnLane z l
                              | otherwise = []
 where
  needed ti = any ((==) ti . (*60) )

-- | For every lane in the lane list, create a zombie
putZombieOnLane :: [Zombie] -> [Lane] -> [Zombie]
putZombieOnLane zombies l = concatMap updateLane l
 where updateLane lane = map (\z -> z {zombiepos=(getXZombie z,lane)} ) zombies

-- | use Time to filter Phases that ended
filterPhases :: Time -> [Phases] -> [Phases]
filterPhases t p | length p > 1 = del (p !! 1)
                 | otherwise = p
 where
  del (Phases start _ _) | (start * 60) < t = tail p
                         | otherwise = p

getCurrentPhaseSpawns :: [Phases] -> [Spawn]
getCurrentPhaseSpawns [] = []
getCurrentPhaseSpawns h = getSpawns $ head h



  -- round10 x = (fromInteger $ round $ x *10) / 10