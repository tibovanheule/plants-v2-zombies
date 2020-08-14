-- | Types module defines data types of the game, constructors and getters.
module Types where
import Graphics.Gloss (Picture)

-- | All type synonyms
type Coordinate = (Float, Float)
type Life = Int
type Damage = Int
type Speed = Float
type Time = Float
type Error = String
type Energy = Int
type CurrLevel = Int
type Direction = Coordinate

-- |Diffrent state of a game
data State = Ongoing | Won | Lost | Menu
             deriving (Eq, Show, Read)

-- | diffrent phases of a level game
data PhaseType = ZombiePhase | BuildingPhase | EndPhase
                deriving (Eq,Show,Read)

-- | Possible types of plants
data PlantType = Sunflower | Peashooter | Walnut
             deriving (Eq, Show, Read)

-- | Possible ypes of zombies
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
                        , phaseSpawns :: [Spawn]
} deriving (Show)

data Spawn = Spawn { wanneer :: [Time]
                    ,spawnLanes :: [Coordinate]
                    ,spawnzombies :: [Zombie]
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
                   , levelmap :: Map
} deriving (Show)

data Map = Map { wall :: [(Coordinate,Coordinate)]
                   , homes :: [Coordinate]
                   , graves :: [(Coordinate,Char)]
} deriving (Show)

-- | World Data type, keeps all level options (as read by the parser) and keep state
data World = World {
                     -- time keeps
                    worldtime   :: Time
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
                walnutimage :: Picture,
                graveimage :: Picture,
                homeimage :: Picture,
                backgroundimage :: Picture
}



----- Constructors

-- | Create a zombie Dog
createDog :: Zombie                   
createDog = Zombie Dog 2 (9,0) 3 1

-- | Create a zombie Farmer
createFarmer :: Zombie
createFarmer = Zombie Farmer 3 (9,0) 4 (1/3)

-- | Create a zombie citizen
createCitizen :: Zombie
createCitizen = Zombie Citizen 3 (9,0) 2 (1/3)

createSunflower :: Coordinate -> Plant
createSunflower c = Plant Sunflower 1 c 0 [] 3

createPeaShooter :: Coordinate -> Plant
createPeaShooter c = Plant Peashooter 1 c 0 [] 6

createWalnut :: Coordinate -> Plant
createWalnut c = Plant Walnut 5 c 0 [] 6

createPea :: Coordinate -> Direction -> Pea
createPea c  = Pea c 0.5 1

-- | creeër een spel with one level (selected level is that given level and is a Just)
createGame :: Level -> World
createGame l = World 0 (Just l) Ongoing [] 0

-- | create a game with multiple levels, selected level is a Nothing
createPossibleGame :: [Level] -> World
createPossibleGame l = World 0 Nothing Menu l 0
                   
---- SPECIAL Getters
-- | Get the next phase
getNextPhase :: [Phases] -> Phases
getNextPhase [] = Phases 0 EndPhase []
getNextPhase [h]= h
getNextPhase (_:t) = head t

-- | Time of the EndPhase
getEnd :: [Phases] -> Float
getEnd = (*60) . getTimePhase . last
 where getTimePhase (Phases t _ _ ) = t