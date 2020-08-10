module LevelParser (levelParser) where
import Parser
import Data.Char (isDigit, isAlpha, isHexDigit, isUpper, isLower)
import Types
import Control.Monad
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

-- | Parse whitespace
whiteParser :: Parser String
whiteParser = many $ token '\t' <|> token ' '

-- | Parse whitespace, endline and then whitespace
whiteEndWhiteParser :: Parser String
whiteEndWhiteParser = whiteParser >> endlineParser >> whiteParser

-- | Parse title of level
titleParser :: Parser String
titleParser = some $ letterParser <|> digitParser <|> symbolParser <|> token ' ' <|> token '\t'

-- | Parse difficulty of a level
difficultyParser :: Parser Float
difficultyParser = do token '('
                      f <- some $ digitParser <|> token '.'
                      token ')'
                      let diff = read f :: Float
                      when ( diff < 0 || diff > 1) (geefError "Invalid difficulty: schould be between 0 and 1.")
                      return (read f:: Float)

-- | skip a comma and whitespace
kommasep :: Parser String
kommasep = many $ token ',' <|> token '\t' <|> token ' '

-- | parse a list of seeds
seedsParser :: Parser [PlantType]
seedsParser = many seedParser

-- | parse one seed
seedParser :: Parser PlantType
seedParser  = kommasep >> (string "Sunflower" <|> string "Peashooter" <|> string "Walnut") >>= \x -> return (read x ::PlantType)

-- | Parse timestamp and return total of seconds.
timeParser :: Parser Time
timeParser = do minutes <- some digitParser
                token ':'
                seconds <- some digitParser
                let min = read minutes :: Float
                    sec = read seconds :: Float
                when (sec < 0 || sec > 59) (geefError "Invalid timestamp: seconds must be between 0 and 59.")
                when (min < 0 || min > 99) (geefError "Invalid timestamp: minutes must be between 0 and 59.")
                return ((min *60) + sec)

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
phaseParser = do whiteEndWhiteParser
                 timestamp <- timeParser
                 whiteEndWhiteParser
                 optional $ token '-'
                 whiteParser
                 name <- orParser [zombiePhaseParser,buildingPhaseParser,endPhaseParser]
                 spawns <- many spawnParser
                 return (Phases timestamp name spawns)

-- | Parses sentence like "home A"
laneParser :: Parser [Lane]
laneParser = string "home" >> whiteParser >> digitParser >>= \lane -> return [read [lane] ::Float]

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
                                Just t -> [0.0] >>= \i -> wanneer t >>= \j -> return (i+j)
                      lanes = case nested of
                                   Nothing -> lane
                                   Just t -> case spawnLanes t of
                                                  [] -> lane
                                                  _ -> spawnLanes t
                      zombies = case nested of
                                     Nothing -> []
                                     Just t -> spawnzombies t
                  return (Spawn runs lanes zombies)


--- after  5 seconds {
afterParser :: Parser Spawn
afterParser = do time <- some $ digitParser <|> token '.'
                 whiteParser
                 string "seconds"
                 whiteParser
                 token '{'
                 whiteEndWhiteParser
                 nested <- optional spawnParser
                 let runs = case nested of
                                Nothing -> [read time ::Float]
                                Just t -> [read time ::Float] >>= \i -> wanneer t >>= \j -> return (i+j)
                     lanes = case nested of
                                   Nothing -> []
                                   Just t -> spawnLanes t
                     zombies = case nested of
                                     Nothing -> []
                                     Just t -> spawnzombies t
                 whiteEndWhiteParser
                 token '}'
                 whiteEndWhiteParser
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
                      whiteEndWhiteParser
                      nested <- optional spawnParser
                      let run = [read times :: Int] >>= \timesInt -> [read sec ::Float] >>= \secInt -> take timesInt [0,secInt..]
                          runs = case nested of
                                    Nothing -> run
                                    Just t -> run >>= \i -> wanneer t >>= \j -> return (i+j)
                          lanes = case nested of
                                       Nothing -> []
                                       Just t -> spawnLanes t
                          zombies = case nested of
                                         Nothing -> []
                                         Just t -> spawnzombies t
                      whiteEndWhiteParser
                      token '}'
                      endlineParser
                      return (Spawn runs lanes zombies)

spawnParser :: Parser Spawn
spawnParser = do whiteEndWhiteParser
                 whatHappend <-  string "on" <|> string "after" <|> string "every" <|> string "Bucket" <|> string "Citizen" <|> string "Farmer" <|> string "Dog"
                 whiteEndWhiteParser
                 case whatHappend of
                       "on" -> onLaneParser
                       "after" -> afterParser
                       "every" -> everySpawnParser
                       "Dog" -> return (Spawn [0] [] [createDog])
                       "Citizen" -> return (Spawn [0] [] [createCitizen])
                       "Farmer" -> return (Spawn [0] [] [createFarmer])
                       "Bucket" -> bucketParser
                       _ -> geefError "Not in case"

bucketParser :: Parser Spawn
bucketParser = do what <- string "Citizen" <|> string "Farmer" <|> string "Dog"
                  let zombie = case what of
                                    "Dog" -> createDog
                                    "Citizen" ->  createCitizen
                                    "Farmer" -> createFarmer
                  return (Spawn [0] [] [zombie { zombielife = zombielife zombie + 2 }])

mapParser :: Parser Map
mapParser = do line1 <- maplineParser 0
               line2 <- maplineParser 1
               line3 <- maplineParser 2
               line4 <- maplineParser 3
               line5 <- maplineParser 4
               line6 <- maplineParser 5
               return (Map [] [] [] )

maplineParser :: Float -> Parser Map
maplineParser y = do cells <- count 9 cell
                     endlineParser
                     cellToMap 6 cells (Map [] [] [])
                     return  (Map [] [] [] )
 where cell = hex <|> token 'X' <|> grave
       hex = spot (\x -> isHexDigit x && (isLower x || isDigit x))
       grave = spot isUpper

cellToMap :: Float -> String -> Map -> Map
cellToMap _ [] map = map
cellToMap y [x] map = (map2 x (0,y))
cellToMap y s@(x:xs) map = cellToMap xs (map2 x (length s - 1, y))
 where map2 x c | isHexDigit x && (isLower x || isDigit x) = map { walls = newwalls x c }
       newwalls x c = map (add c) [(1,0),(0,1),(-1,0),(0,-1)]
                       where add (x,y) (x',y') = (x+x',y+y')

-- | Parse a level
levelParser :: Parser Level
levelParser = do title <- titleParser
                 diff <- difficultyParser
                 endlineParser
                 seeds <- seedsParser
                 endlineParser
                 map <- mapParser
                 phases <- some phaseParser
                 return $ Level title diff seeds [] [] phases 10 Nothing