-- | Implementation of the game logic.
module Game where
import           Types
import           LevelParser
import           Parser
import SetupParser

-- Parser Calls
-- | Call parser en parse level file contents using levelparser
getLevel :: String -> Either Error Level
getLevel =  parseStatement levelParser

-- | Call parser en parse defence file contents using setupparser
getDefence :: String -> Either Error [Plant]
getDefence = parseStatement setupParser

--- GAME LOGIC

-- | filter away dead zombies
dead :: [Zombie] -> [Zombie]
dead = filter isAlive
       where isAlive (Zombie _ l _ _ _) = l > 0

-- | move a zombie using his speed, only if possible.
moveZombie :: [Plant] -> Zombie -> Zombie
moveZombie plants z@(Zombie _ _ c@(x,y) _ speed) = z { zombiepos=pos }
               where pos = case zombieBeforePlant plants (x-speed,y) of
                                True -> c
                                False -> (x-speed,y)

zombieBeforePlant :: [Plant] -> Coordinate -> Bool
zombieBeforePlant p (x,y) = any check p
                            where check (Plant _ _ (x',y') _ _) = x' >= x && y' == y

movePea :: Coordinate -> Speed -> Coordinate
movePea p@(x,y) speed = p

isHit :: Coordinate -> Coordinate -> Bool
isHit (x,_) (x',_) = x >= x'

-- | Reduce a Life with damage x
damage :: Life -> Damage -> Life
damage = (-)

-- | geeft de staat van spel terug
isWon :: PhaseType -> [Zombie] -> State
isWon EndPhase [] = Won
isWon EndPhase [a] = Lost
isWon _ z = zombiePosCheck z

zombiePosCheck :: [Zombie] -> State
zombiePosCheck zombies | any (\z -> getXZombie z <= 0) zombies = Lost
                       | otherwise = Ongoing

changeWorld :: Time -> Energy -> Level -> World
changeWorld time en l@(Level _ _ _ z p phases) = World (time+1) level (isWon (getCurrentPhaseType $ filterPhases time phases) z) [] (calcEnergy en p) 0
                                         where zom =  dead $ map (moveZombie p) z ++ spawn time phases
                                               plant = shootPlants p
                                               level = Just (l { zombies=zom, phase= filterPhases time phases, plants=plant})

-- | calculates total amount of energy, uses checkSunflower function ()
calcEnergy :: Energy -> [Plant] -> Energy
calcEnergy en p = en + (sum $ map checkSunflower p)

-- TODO FIX conflict met schoot !
-- | check if 3 seconds has passed, and add energy if needed
checkSunflower :: Plant -> Energy
checkSunflower (Plant Sunflower _ _ lastshot _) | lastshot >= 180 = 1
                                                | otherwise = 0
checkSunflower _ = 0

shootPlants :: [Plant] -> [Plant]
shootPlants = map shoot

shoot :: Plant -> Plant
shoot p@(Plant Sunflower _ _ t _) | t >= 180 = p {lastshot=0}
                                         | otherwise = p {lastshot=lastshot(p)+1}
shoot p@(Plant Peashooter _ _ t _) | t >= 120 = p {lastshot=0,shots=createPea (plantpos(p)):shots(p)}
                                          | otherwise =  p {lastshot=lastshot(p)+1}
schoot p = p
                                 
-- | Get all spawns of zombies out of a phase
spawn :: Time -> [Phases] -> [Zombie]
spawn t = checkSpawns t . getCurrentPhaseSpawns

-- controleerd de spaws en een lijst van zombies maken als dit nodig blijkt
checkSpawns :: Time -> [Spawn] -> [Zombie]
checkSpawns t = concatMap (createZombies t)

--is een spawn nodig?

--TODO Dit klopt niet ????
-- een lijst van zombies maken als dit nodig blijkt
createZombies :: Time -> Spawn -> [Zombie]
createZombies t s@(Spawn r l z) | needed t r = laneZombie z l
                                | otherwise = []
 where
  needed :: Time -> [Time] -> Bool
  needed ti = any (ti ==)


-- zet zombie op elke lane
laneZombie :: [Zombie] -> [Lane] -> [Zombie]
laneZombie z = concatMap (updateLane z) 

--  zet zombie op correcte lane
updateLane :: [Zombie] -> Lane  -> [Zombie]
updateLane zombies l = map (\z -> z {zombiepos=(getXZombie z,l)} ) zombies

-- | use Time to filter Phases that ended
filterPhases :: Time -> [Phases] -> [Phases]
filterPhases t p = filter (\x -> getDuration x < t) p

getCurrentPhaseType :: [Phases] -> PhaseType
getCurrentPhaseType [] = EndPhase
getCurrentPhaseType [h] = getPhaseType h

getCurrentPhaseSpawns :: [Phases] -> [Spawn]
getCurrentPhaseSpawns [] = []
getCurrentPhaseSpawns [h] = getSpawnsType h