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
type Wall = (Coordinate,Coordinate)

-- | Diffrent state of a game
data State = Ongoing | Won | Lost | Menu
             deriving (Eq, Show, Read)

-- | diffrent phases of a level game
data PhaseType = ZombiePhase | BuildingPhase | EndPhase
                deriving (Eq,Show,Read)

-- | Possible types of plants
data PlantType = Sunflower | Peashooter | Walnut
             deriving (Eq, Show, Read)

-- | Possible types of zombies
data ZombieType = Citizen | Farmer | Dog 
             deriving (Eq, Show, Read)

-- | Data record for zombies (stores: type, life, position,damage,speed,and last time it attacked a plant)
data Zombie = Zombie { zombietype   :: ZombieType
                   , zombielife :: Life
                   , zombiepos :: Coordinate
                   , zombiedamage :: Damage
                   , zombiespeed :: Speed
                   , zombielastattack :: Time
                   } deriving (Eq,Show)

-- | Data record for plants (stores: type, life, position,last shot pea,shot peas and the cost to buy a plant)
data Plant = Plant { planttype   :: PlantType
                  , plantxp :: Life
                  , plantpos :: Coordinate
                  , lastshot :: Time
                  , shots :: [Pea]
                  , cost :: Int
                  } deriving (Eq,Show)

-- | Data record for Peas (stores: position,speed,damage and direction)
data Pea = Pea { peapos :: Coordinate
  , peaspeed :: Speed
  , peadamage :: Damage
  , peadirection :: Direction
  } deriving (Eq, Show)

-- | Data record for phases (stores: starttime, type, all spawns)
data Phases = Phases {  start :: Time
                        , phaseType :: PhaseType
                        , phaseSpawns :: [Spawn]
} deriving (Show)

-- | Data record for spawns (stores: firing times, homes to spawn from and wich zombies to spawn)
data Spawn = Spawn { wanneer :: [Time]
                    ,spawnLanes :: [Coordinate]
                    ,spawnzombies :: [Zombie]
} deriving (Show)

-- | Level keeps information about a level (stores: title, difficulty, seeds (allowed to plant),active zombies, active plants, phases, engery, chosenplant from the store, map (maze))
-- | only information of the level itself no state
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

-- | Data record for Map (stores: walls, homes and graves)
data Map = Map { wall :: [Wall]
                   , homes :: [Coordinate]
                   , graves :: [(Coordinate,Char)]
} deriving (Show)

-- | World Data type, keeps all level options (as read by the parser) and keep state
-- stores: time, chosenlevel, game state, all parsed levels, currrent index of chosen level
data World = World {
                     -- time keeps
                    worldtime   :: Time
                   , chosenLevel :: Maybe Level
                   , state :: State
                   , plevels :: [Level]
                   , currlevel :: CurrLevel
                   } deriving (Show)

-- | Data record for Images
-- To avoid use of unsafepPerformIO, initialized in main and passed down to the gui functions
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
createDog = Zombie Dog 2 (9,0) 3 1 0

-- | Create a zombie Farmer
createFarmer :: Zombie
createFarmer = Zombie Farmer 3 (9,0) 4 (1/3) 0

-- | Create a zombie citizen
createCitizen :: Zombie
createCitizen = Zombie Citizen 3 (9,0) 2 (1/3) 0

-- | Create a sunflower
createSunflower :: Coordinate -> Plant
createSunflower c = Plant Sunflower 1 c 0 [] 3

-- | Create a pea schooter
createPeaShooter :: Coordinate -> Plant
createPeaShooter c = Plant Peashooter 1 c 0 [] 6

-- | Create a walnut
createWalnut :: Coordinate -> Plant
createWalnut c = Plant Walnut 5 c 0 [] 6

-- | Create a pea (shot)
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
getEnd = start . last