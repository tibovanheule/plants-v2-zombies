module LevelParser where
import Parser
import Data.Char (isDigit, isAlpha)
import Types
import Control.Monad (void);
{-
some = 1 of meer
many = 0 of meer
-}

-- | parse a letter
letterParser :: Parser Char
letterParser =  spot isAlpha

-- | parse a symbol
symbolParser :: Parser Char
symbolParser =   token '!' <|> token '&' <|> token '?'

-- | Parse one whitespace skip the rest be
whiteParser :: Parser Char
whiteParser = do f <- token ' '
                 many $ token ' ' <|> token '\t'
                 return f

-- | Parse title of level
titleParser :: Parser String
titleParser = some $ letterParser <|> digitParser <|> symbolParser <|> whiteParser

-- | Parse difficulty of a level
difficultyParser :: Parser Float
difficultyParser = do token '('
                      f <- some $ digitParser <|> token '.'
                      token ')'
                      return (read f:: Float)

-- | skip a comma and whitespace
kommasep :: Parser a -> Parser a
kommasep p = do
              many $ token ',' <|> whiteParser
              p

seedsParser :: Parser [PlantType]
seedsParser = do f <- seedParser
                 l <- many (kommasep seedParser)
                 return $ f:l

seedParser :: Parser PlantType
seedParser  = do x <- string "Sunflower" <|> string "Peashooter" <|> string "Walnut"
                 return (read x ::PlantType)

timeParser :: Parser Time
timeParser = do minutes <- some digitParser
                token ':'
                seconds <- some digitParser
                return (((read minutes::Float) *60) + read seconds::Float)

zombiePhaseParser :: Parser PhaseType
zombiePhaseParser = do optional whiteParser
                       token '-'
                       whiteParser
                       string "Zombie Phase"
                       return ZombiePhase

buildingPhaseParser :: Parser PhaseType
buildingPhaseParser = do whiteParser
                         token '-'
                         whiteParser
                         string "Building Phase"
                         return BuildingPhase

endPhaseParser :: Parser PhaseType
endPhaseParser = return EndPhase

laneParser :: Parser [Lane]
laneParser =  do string "lane"
                 optional whiteParser
                 lane <- digitParser
                 let laneInt = read [lane]
                 let test = laneInt <= 6
                 case test of
                      True -> return [laneInt]
                      _ -> geefError "lane groter dan 6"
                 return [laneInt]

everyParser :: Parser [Lane]
everyParser = do test <- string "every"
                 optional whiteParser
                 string "lane"
                 return [1,2,3,4,5,6]

onLaneParser :: Parser Spawn
onLaneParser = do many whiteParser 
                  optional $ string "on"
                  lane <- laneParser <|> everyParser
                  many $ whiteParser <|> letterParser
                  token '{'
                  many whiteParser 
                  nested <- spawnParser
                  many whiteParser
                  token '}'
                  many $ whiteParser <|> letterParser
                  endlineParser
                  let runs = [0.0] >>= \i -> getTimes nested >>= \j -> return (i+j)
                  let lanes = case getLane nested of
                                   [] -> lane
                                   _ -> getLane nested
                  let zombies = getZombie nested
                  return (Spawn runs lanes zombies)

--- after  5 seconds {
afterParser :: Parser Spawn
afterParser = do many whiteParser
                 time <- some $ digitParser <|> token '.'
                 many $ whiteParser <|> letterParser
                 token '{'
                 many whiteParser <|> endlineParser
                 nested <- spawnParser
                 let runs = [read time ::Float] >>= \i -> getTimes nested >>= \j -> return (i+j)
                 let lanes = getLane nested
                 let zombies = getZombie nested
                 many whiteParser <|> endlineParser
                 token '}'
                 many whiteParser <|> endlineParser
                 return (Spawn runs lanes zombies)
                 
endLineSep :: Parser a -> Parser a
endLineSep p = do
              optional whiteParser
              endlineParser
              p

zombieParser :: Parser Zombie
zombieParser = do zombie <-  string "Dog" <|> string "Citizen" <|> string "Farmer" <|> string "Bucket"
                  case zombie of
                    "Bucket" -> do optional whiteParser
                                   z <- zombieParser
                                   return (z {zombiexp=zombiexp(z)+2})
                    "Dog" -> return createDog
                    "Citizen" -> return createCitizen 
                    "Farmer" -> return createFarmer


zombiesParser :: Parser [Zombie]
zombiesParser = many (endLineSep zombieParser)

everySpawnParser :: Parser Spawn
everySpawnParser = do optional whiteParser
                      optional $ string "every"
                      sec <- many $ digitParser <|> token '.'
                      many $ whiteParser <|> letterParser
                      times <- some digitParser
                      many $ whiteParser <|> letterParser
                      let timesInt = read times
                      let secInt = read sec ::Float
                      let run = take timesInt [0,secInt..]
                      token '{'
                      many whiteParser
                      endlineParser
                      many whiteParser
                      nested <- spawnParser
                      let runs = run >>= \i -> getTimes nested >>= \j -> return (i+j)
                      let lanes = getLane nested
                      let zombies = getZombie nested
                      endlineParser
                      many whiteParser
                      token '}'
                      endlineParser
                      return (Spawn runs lanes zombies)

spawnParser :: Parser Spawn
spawnParser = do many whiteParser
                 endlineParser
                 many $ token '\t'
                 many whiteParser
                 whatHappend <-  string "on" <|> string "after" <|> string "every" <|> string "Bucket" <|> string "Citzen" <|> string "Farmer" <|> string "Dog"
                 case whatHappend of
                      "on" -> onLaneParser
                      "after" -> afterParser
                      "every" -> everySpawnParser
                      "Dog" -> do list <- zombiesParser
                                  return (Spawn [0] [] (createDog:list))
                      "Farmer" -> do list <- zombiesParser
                                     let zombies = createFarmer:list
                                     return (Spawn [0] [] zombies)
                      "Citizen" -> do list <- zombiesParser
                                      let zombies = createCitizen:list
                                      return(Spawn [0] [] zombies)
                      "Bucket" -> do whiteParser ; z <- zombieParser
                                     let z = (z {zombiexp=zombiexp(z)+2})
                                     list <- zombiesParser
                                     let zombies = z:list
                                     return (Spawn [0] [] zombies)
phaseParser :: Parser Phases
phaseParser = do many whiteParser
                 endlineParser
                 timestamp <- timeParser
                 name <- orParser [zombiePhaseParser,buildingPhaseParser,endPhaseParser]
                 endlineParser
                 spawns <- many spawnParser
                 return (Phases timestamp name spawns)

levelParser :: Parser Level
levelParser = do
                title <- titleParser
                diff <- difficultyParser
                endlineParser
                seeds <- seedsParser
                endlineParser
                phases <- some phaseParser
                return (Level title diff seeds [] [] phases)