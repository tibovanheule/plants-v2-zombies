module LevelParser (levelParser) where
import Parser
import Data.Char (isDigit, isAlpha)
import Types
{- for reference
some = 1 of meer
many = 0 of meer
-}

-- | parse a letter
letterParser :: Parser Char
letterParser = spot isAlpha

-- | parse a symbol
symbolParser :: Parser Char
symbolParser = token '!' <|> token '&' <|> token '?'

-- | Parse one whitespace skip the rest be
whiteParser :: Parser String
whiteParser = many $ token '\t' <|> token ' '

-- | Parse title of level
titleParser :: Parser String
titleParser = some $ letterParser <|> digitParser <|> symbolParser <|> token ' ' <|> token '\t'

-- | Parse difficulty of a level
difficultyParser :: Parser Float
difficultyParser = do token '('
                      f <- some $ digitParser <|> token '.'
                      token ')'
                      return (read f:: Float)

-- | skip a comma and whitespace
kommasep :: Parser a -> Parser a
kommasep p = (many $ token ',' <|> token '\t' <|> token ' ') >> p

-- | parse a list of seeds
seedsParser :: Parser [PlantType]
seedsParser = do f <- seedParser
                 l <- many (kommasep seedParser)
                 return $ f:l

-- | parse one seed
seedParser :: Parser PlantType
seedParser  = (string "Sunflower" <|> string "Peashooter" <|> string "Walnut") >>= \x -> return (read x ::PlantType)

-- | Parse timestamp and return total of seconds.
timeParser :: Parser Time
timeParser = do minutes <- some digitParser
                token ':'
                seconds <- some digitParser
                return (((read minutes::Float) *60) + read seconds::Float)

-- | Parse a Zombie Phase
zombiePhaseParser :: Parser PhaseType
zombiePhaseParser = string "Zombie Phase" >> return ZombiePhase

-- | Parse a Building Phase
buildingPhaseParser :: Parser PhaseType
buildingPhaseParser = string "Building Phase" >> return BuildingPhase

-- | Parse a EndPhase Phase
endPhaseParser :: Parser PhaseType
endPhaseParser = return EndPhase

-- | Parse all Phases and there spawns
phaseParser :: Parser Phases
phaseParser = do whiteParser
                 endlineParser
                 timestamp <- timeParser
                 endlineParser
                 whiteParser
                 optional $ token '-'
                 whiteParser
                 name <- orParser [zombiePhaseParser,buildingPhaseParser,endPhaseParser]
                 spawns <- many spawnParser
                 return (Phases timestamp name spawns)

-- | Parses sentence like "home A"
laneParser :: Parser [Lane]
laneParser =  do string "home"
                 whiteParser
                 lane <- digitParser
                 return [read [lane] ::Float]

-- | Parses sentences like " on every home"
everyParser :: Parser [Lane]
everyParser = string "every" >> whiteParser >> string "home" >> return [1,2,3,4,5,6]

-- | Parses sentences like on every lane ...
onLaneParser :: Parser Spawn
onLaneParser = do lane <- everyParser <|> laneParser
                  whiteParser
                  token '{'
                  whiteParser
                  nested <- optional spawnParser
                  whiteParser
                  endlineParser
                  whiteParser
                  token '}'
                  endlineParser
                  let runs = case nested of
                                Nothing -> [0]
                                Just t -> [0.0] >>= \i -> getTimes t >>= \j -> return (i+j)
                      lanes = case nested of
                                   Nothing -> lane
                                   Just t -> case getLanes t of
                                                  [] -> lane
                                                  _ -> getLanes t
                      zombies = case nested of
                                     Nothing -> []
                                     Just t -> getZombie t
                  return (Spawn runs lanes zombies)


--- after  5 seconds {
afterParser :: Parser Spawn
afterParser = do time <- some $ digitParser <|> token '.'
                 whiteParser
                 string "seconds"
                 whiteParser
                 token '{'
                 whiteParser
                 endlineParser
                 whiteParser
                 nested <- optional spawnParser
                 let runs = case nested of
                                Nothing -> [read time ::Float]
                                Just t -> [read time ::Float] >>= \i -> getTimes t >>= \j -> return (i+j)
                     lanes = case nested of
                                   Nothing -> []
                                   Just t -> getLanes t
                     zombies = case nested of
                                     Nothing -> []
                                     Just t -> getZombie t
                 whiteParser
                 endlineParser
                 token '}'
                 whiteParser
                 endlineParser
                 return (Spawn runs lanes zombies)

everySpawnParser :: Parser Spawn
everySpawnParser = do whiteParser
                      sec <- many $ digitParser <|> token '.'
                      whiteParser
                      string "seconds"
                      whiteParser
                      string "for"
                      whiteParser
                      times <- some digitParser
                      whiteParser
                      string "times"
                      whiteParser
                      token '{'
                      whiteParser
                      endlineParser
                      whiteParser
                      nested <- optional spawnParser
                      let run = [read times :: Int] >>= \timesInt -> [read sec ::Float] >>= \secInt -> take timesInt [0,secInt..]
                          runs = case nested of
                                    Nothing -> run
                                    Just t -> run >>= \i -> getTimes t >>= \j -> return (i+j)
                          lanes = case nested of
                                       Nothing -> []
                                       Just t -> getLanes t
                          zombies = case nested of
                                         Nothing -> []
                                         Just t -> getZombie t
                      whiteParser
                      endlineParser
                      whiteParser
                      token '}'
                      endlineParser
                      return (Spawn runs lanes zombies)

spawnParser :: Parser Spawn
spawnParser = do whiteParser
                 endlineParser
                 whiteParser
                 -- TODO vervang door gewone orp  rser
                 whatHappend <-  string "on" <|> string "after" <|> string "every" <|> string "Bucket" <|> string "Citizen" <|> string "Farmer" <|> string "Dog"
                 whiteParser
                 endlineParser
                 whiteParser
                 case whatHappend of
                       "on" -> onLaneParser
                       "after" -> afterParser
                       "every" -> everySpawnParser
                       "Dog" -> return (Spawn [0] [] [createDog])
                       "Citizen" -> return (Spawn [0] [] [createCitizen])
                       "Farmer" -> return (Spawn [0] [] [createFarmer])
                       "Bucket" -> return (Spawn [0] [] [])
                       _ -> geefError "Not in case"

-- | Parse a level
levelParser :: Parser Level
levelParser = do title <- titleParser
                 diff <- difficultyParser
                 endlineParser
                 seeds <- seedsParser
                 endlineParser
                 phases <- some phaseParser
                 return $ Level title diff seeds [] [] phases 10 Nothing
