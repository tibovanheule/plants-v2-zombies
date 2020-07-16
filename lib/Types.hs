-- | Types module defines data types of the game, constructors and getters.
module Types where
import           Graphics.Gloss                     (Picture)

type Coordinate = (Float, Int)
type Life = Int
type Damage = Int
type Lane = Int
type Speed = Float
type Time = Float
type Error = String
type Energy = Int


data Images =  Images
    { back :: Picture
    } deriving (Show)

data State = Ongoing | Won | Lost | Menu
             deriving (Eq, Show, Read)
             
data PhaseType = ZombiePhase | BuildingPhase | EndPhase
                deriving (Eq,Show,Read)
             
data PlantType = Sunflower | Peashooter | Walnut
             deriving (Eq, Show, Read)
data ZombieType = Citizen | Farmer | Dog 
             deriving (Eq, Show, Read)

data Zombie = Zombie { zombietype   :: ZombieType
                   , zombiexp :: Life
                   , zombiepos :: Coordinate
                   , zombiedamage :: Damage
                   , zombiespeed :: Speed
                   } deriving (Eq,Show)

data Plant = Plant { planttype   :: PlantType
                  , plantxp :: Life
                  , plantpos :: Coordinate
                  , lastshot :: Time
                  , shots :: [Pea]
                  } deriving (Eq,Show)
                  
data Pea = Pea { peapos :: Coordinate
  , peaspeed :: Speed
  , peadamage :: Damage
  } deriving (Eq, Show)
  
data Phases = Phases {  start :: Time
                        , phaseType :: PhaseType
                        , spawns :: [Spawn]
} deriving (Show)

data Spawn = Spawn { wanneer :: [Time]
                    ,lanes :: [Lane]
                    ,types :: [Zombie]
} deriving (Show)
                  
data Level = Level { levelfile :: String
                   , difficulty :: Float
                   , seeds :: [PlantType]
                   , zombies :: [Zombie]
                   , plants   :: [Plant]
                   , phase :: [Phases]
} deriving (Show)
                  
data World = World { time   :: Time
                   , curlevel :: Maybe Level
                   , state :: State
                   , plevels :: [Level]
                   , engery :: Energy
                   } deriving (Show)
                   
----- Constructors
createDog :: Zombie                   
createDog = Zombie Dog 2 (9,0) 3 0.1

createFarmer :: Zombie
createFarmer = Zombie Farmer 3 (9,0) 4 (1/30)

createCitizen :: Zombie
createCitizen = Zombie Citizen 3 (9,0) 2 (1/30)

createSunflower :: Plant
createSunflower = Plant Sunflower 1 (0,0) 0.0 []

createPeaShooter :: Plant
createPeaShooter = Plant Peashooter 1 (0,0) 0.0 []

createPea :: Coordinate -> Pea
createPea c = Pea c 0.5 1

-- creeër een spel
createGame :: Level -> World
createGame l = World 0 (Just l) Ongoing [] 0

createPossibleGame :: [Level] -> World
createPossibleGame l = World 0 Nothing Menu l 0
                   
---- GETTERS
getTimes :: Spawn -> [Time]
getTimes (Spawn t _ _) = t 

getZombie :: Spawn -> [Zombie]
getZombie (Spawn _ _ z) = z

getLane :: Spawn -> [Lane]
getLane (Spawn _ l _) = l

-- krijg de staat van het spel
getState :: World -> State
getState (World _ _ s _ _) = s

-- krijg tijd van het spel
getTime :: World -> Time
getTime (World t _ _ _ _) = t

-- krijg duration van een phase
getDuration :: Phases -> Time
getDuration  (Phases t _ _) = t

-- krijg x-coordinaat van zombie
getXZombie :: Zombie -> Float
getXZombie (Zombie _ _ (x,_) _ _) = x

-- krijg de type van een phase
getPhaseType :: Phases-> PhaseType
getPhaseType (Phases _ t _) = t

-- krijg de spawns van een phase
getSpawnsType :: Phases-> [Spawn]
getSpawnsType (Phases _ _ s) = s