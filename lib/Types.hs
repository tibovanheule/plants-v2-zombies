-- | Types module defines data types of the game, constructors and getters.
module Types where
import Graphics.Gloss (Picture)

-- | All type synonyms
type Coordinate = (Float, Float)
type Life = Int
type Damage = Int
type Lane = Float
type Speed = Float
type Time = Float
type Error = String
type Energy = Int
type CurrLevel = Int
type Direction = Coordinate

left, right, up, down :: Speed -> Direction
left speed = (-speed,0)
right speed = (speed,0)
up speed = (0,speed)
down speed= (0,-speed)

data State = Ongoing | Won | Lost | Menu
             deriving (Eq, Show, Read)
             
data PhaseType = ZombiePhase | BuildingPhase | EndPhase
                deriving (Eq,Show,Read)
             
data PlantType = Sunflower | Peashooter | Walnut
             deriving (Eq, Show, Read)
data ZombieType = Citizen | Farmer | Dog 
             deriving (Eq, Show, Read)

data Zombie = Zombie { zombietype   :: ZombieType
                   , zombielife :: Life
                   , zombiepos :: Coordinate
                   , zombiedamage :: Damage
                   , zombiespeed :: Speed
                   } deriving (Eq,Show)

data Plant = Plant { planttype   :: PlantType
                  , plantxp :: Life
                  , plantpos :: Coordinate
                  , lastshot :: Time
                  , shots :: [Pea]
                  , cost :: Int
                  } deriving (Eq,Show)
                  
data Pea = Pea { peapos :: Coordinate
  , peaspeed :: Speed
  , peadamage :: Damage
  , peadirection :: Direction
  } deriving (Eq, Show)

data Phases = Phases {  start :: Time
                        , phaseType :: PhaseType
                        , spawns :: [Spawn]
} deriving (Show)

data Spawn = Spawn { wanneer :: [Time]
                    ,lanes :: [Lane]
                    ,types :: [Zombie]
} deriving (Show)

-- | Level keeps information about a level
data Level = Level { title :: String
                   , difficulty :: Float
                   , seeds :: [PlantType]
                   , zombies :: [Zombie]
                   , plants   :: [Plant]
                   , phase :: [Phases]
                   , energy :: Energy
                   , chosenplant :: Maybe PlantType
} deriving (Show)

data Map = Map { wall :: [(Coordinate,Coordinate)]
                   , homes :: [(Coordinate,Char)]
                   , graves :: [(Coordinate,Char)]
} deriving (Show)

-- | World Data type, keeps all level options (as read by the parser) and keep state
data World = World {
                     -- time keeps
                    time   :: Time
                   , chosenLevel :: Maybe Level
                   , state :: State
                   , plevels :: [Level]
                   , currlevel :: CurrLevel
                   } deriving (Show)

data Images = Images {
                citizenimage :: Picture ,
                dogimage :: Picture ,
                farmerimage :: Picture ,
                grassimage :: Picture ,
                peashooterimage :: Picture ,
                sunflowerimage :: Picture ,
                walnutimage :: Picture
}



----- Constructors
createDog :: Zombie                   
createDog = Zombie Dog 2 (9,0) 3 1

createFarmer :: Zombie
createFarmer = Zombie Farmer 3 (9,0) 4 (1/3)

createCitizen :: Zombie
createCitizen = Zombie Citizen 3 (9,0) 2 (1/3)

createSunflower :: Coordinate -> Plant
createSunflower c = Plant Sunflower 1 c 0 [] 3

createPeaShooter :: Coordinate -> Plant
createPeaShooter c = Plant Peashooter 1 c 0 [] 6

createWalnut :: Coordinate -> Plant
createWalnut c = Plant Walnut 5 c 0 [] 6

createPea :: Coordinate -> (Speed -> Direction) -> Pea
createPea c d = Pea c 0.5 1 (d (0.5/60))

-- | creeër een spel with one level (selected level is that given level and is a Just)
createGame :: Level -> World
createGame l = World 0 (Just l) Ongoing [] 0

-- | create a game with multiple levels, selected level is a Nothing
createPossibleGame :: [Level] -> World
createPossibleGame l = World 0 Nothing Menu l 0
                   
---- GETTERS
getTimes :: Spawn -> [Time]
getTimes (Spawn t _ _) = t 

getZombie :: Spawn -> [Zombie]
getZombie (Spawn _ _ z) = z

getLanes :: Spawn -> [Lane]
getLanes (Spawn _ l _) = l

getPeas :: Plant -> [Pea]
getPeas (Plant _ _ _ _ p _) = p

-- krijg de staat van het spel
getState :: World -> State
getState (World _ _ s _ _ ) = s

-- krijg tijd van het spel
getTime :: World -> Time
getTime (World t _ _ _ _ ) = t

-- krijg x-coordinaat van zombie
getXZombie :: Zombie -> Time
getXZombie (Zombie _ _ (x,_) _ _) = x

-- | Get the spawns of a phase
getSpawns :: Phases -> [Spawn]
getSpawns (Phases _ _ s) = s

-- | Get the next phase
getNextPhase :: [Phases] -> Phases
getNextPhase [] = Phases 0 EndPhase []
getNextPhase [h]= h
getNextPhase (_:t) = head t

-- | Time of the EndPhase
getEnd :: [Phases] -> Float
getEnd = (*60) . getTimePhase . last
 where getTimePhase (Phases t _ _ ) = t