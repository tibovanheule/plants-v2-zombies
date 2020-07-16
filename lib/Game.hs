module BlaBla where
import           Types
import Data.Maybe (isNothing)
import           System.Environment
import           System.Exit
import           LevelParser
import           Parser
import SetupParser
import System.IO
import Data.Either (rights)

-- Parser Calls
getLevel :: String -> Either Error Level
getLevel =  parseStatement levelParser

getDefence :: String -> Either Error [Plant]
getDefence = parseStatement setupParser

--- GAME LOGIC

-- verwijder dode
dead :: [Zombie] -> [Zombie]
dead = filter isAlive

-- leeft die nog
isAlive :: Zombie -> Bool
isAlive (Zombie _ l _ _ _) = l > 0

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

damage :: Life -> Damage -> Life
damage = (-)

-- geeft de staat van spel terug
whatState :: PhaseType -> [Zombie] -> State
whatState EndPhase [] = Won
whatState EndPhase [a] = Lost
whatState _ z = zombiePosCheck z

zombiePosCheck :: [Zombie] -> State
zombiePosCheck zombies | any (\z -> getXZombie z <= 0) zombies = Lost
                       | otherwise = Ongoing

changeWorld :: Time -> Energy -> Level -> Game
changeWorld time en l@(Level _ _ _ z p phases) = Game (time+0.1) level (whatState (getCurrentPhaseType $ filterPhases time phases) z) [] (en + calcEnergy p)
                                         where zom =  dead $ map (moveZombie p) z ++ spawn time phases
                                               plant = shootPlants p
                                               level = Just (l { zombies=zom, phase= filterPhases time phases, plants=plant})

calcEnergy :: [Plant] -> Energy
calcEnergy = sum . map checkSunflower

checkSunflower :: Plant -> Energy
checkSunflower (Plant Sunflower _ _ t _) = case t >= 3 of
                                                True -> 1
                                                _ -> 0
checkSunflower (Plant _ _ _ t _) = 0

shootPlants :: [Plant] -> [Plant]
shootPlants = map shoot

shoot :: Plant -> Plant
shoot p@(Plant typ _ _ t _) = case typ of
                                 Sunflower -> case t >= 3 of 
                                                   True -> p {lastshot=0} 
                                                   False -> p {lastshot=lastshot(p)+0.1}
                                 Peashooter -> case t >= 2 of 
                                                    True -> p {lastshot=0,shots=createPea (plantpos(p)):shots(p)} 
                                                    False -> p {lastshot=lastshot(p)+0.1}
                                 _ -> p
                                 
-- zal de uit de phase de spans halen en een lijst van zombies maken als dit nodig blijkt
spawn :: Time -> [Phases] -> [Zombie]
spawn t = checkSpawns t . getCurrentPhaseSpawns

-- controleerd de spaws en een lijst van zombies maken als dit nodig blijkt
checkSpawns :: Time -> [Spawn] -> [Zombie]
checkSpawns t = concatMap (createZombies t)

--is een spawn nodig?
needed :: Time -> [Time] -> Bool
needed ti = any (ti ==)

-- een lijst van zombies maken als dit nodig blijkt
createZombies :: Time -> Spawn -> [Zombie]
createZombies t s@(Spawn r l z)  = case needed t r of
                                        False -> []
                                        True -> laneZombie z l
-- zet zombie op elke lane
laneZombie :: [Zombie] -> [Lane] -> [Zombie]
laneZombie z = concatMap (updateLane z) 

--  zet zombie op correcte lane
updateLane :: [Zombie] -> Lane  -> [Zombie]
updateLane zombies l = map (\z -> z {zombiepos=(getXZombie z,l)} ) zombies

-- haalt verlopen phases weg
filterPhases :: Time -> [Phases] -> [Phases]
filterPhases t p = case getDuration (p!!1) < t of
                        True -> drop 1 p
                        _ -> p

getCurrentPhase :: [Phases] -> Phases
getCurrentPhase = head

getCurrentPhaseType :: [Phases] -> PhaseType
getCurrentPhaseType = getPhaseType . getCurrentPhase

getCurrentPhaseSpawns :: [Phases] -> [Spawn]
getCurrentPhaseSpawns = getSpawnsType . getCurrentPhase

------- TEST GEDEELTE
-- run the test
runTest :: Game -> IO()
runTest game = case getState game of
                    Ongoing -> print game >> tick game
                    Menu -> print "ERROR, state corrupted" >> exitWith (ExitFailure 1)
                    Lost -> putStr "Loss after " >> putStr ( show (getTime game)) >> putStr " seconds" >> exitSuccess
                    Won -> putStr "Victory at " >> print (show (getTime game)) >> exitSuccess

-- Geef een tick aan de wereld, laat runtest weer bepalen wat er dan meot geberuen
tick :: Game -> IO()
tick (Game time (Just l)_ _ e) = runTest $ changeWorld time e l
tick (Game _ Nothing _ _ _) = print "ERROR, no level" >> exitWith (ExitFailure 1)
