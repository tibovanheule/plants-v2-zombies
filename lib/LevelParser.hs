module LevelParser (levelParser) where
import Parser
import Data.Char (isDigit, isAlpha, isHexDigit, isUpper, isLower)
import Types
import Control.Monad
import Data.Maybe
import Numeric (readHex)
import Text.Printf (printf)
import Constants
import Debug.Trace
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
phaseParser :: [(Coordinate,Char)] -> Parser Phases
phaseParser graves = do whiteEndWhiteParser
                        timestamp <- timeParser
                        whiteEndWhiteParser
                        optional $ token '-'
                        whiteParser
                        name <- orParser [zombiePhaseParser,buildingPhaseParser,endPhaseParser]
                        spawns <- many $ spawnParser graves
                        return (Phases timestamp name spawns)

-- | Parses sentence like "home A"
laneParser :: [(Coordinate,Char)] -> Parser [Coordinate]
laneParser graves = string "home" >> whiteParser >> letterParser >>= \home -> case filter ( (==) home . snd) graves of
                                                                                 [] -> geefError "invalid home"
                                                                                 _ -> return (map fst $ filter ( (==) home . snd) graves)
-- | Parses sentences like " on every home"
everyParser :: [(Coordinate,Char)] -> Parser [Coordinate]
everyParser graves = string "every" >> whiteParser >> string "home" >> return (map fst graves)

-- | Parses sentences like on every lane ...
onLaneParser :: [(Coordinate,Char)] -> Parser Spawn
onLaneParser graves =
               do lane <- everyParser graves <|> laneParser graves
                  whiteParser
                  token '{'
                  whiteParser
                  nested <- optional $ spawnParser graves
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
afterParser :: [(Coordinate,Char)] -> Parser Spawn
afterParser graves
            = do time <- some $ digitParser <|> token '.'
                 whiteParser
                 string "seconds"
                 whiteParser
                 token '{'
                 whiteEndWhiteParser
                 nested <- optional $ spawnParser graves
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

everySpawnParser :: [(Coordinate,Char)] -> Parser Spawn
everySpawnParser graves = do whiteParser
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
                             nested <- optional $ spawnParser graves
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

spawnParser :: [(Coordinate,Char)] -> Parser Spawn
spawnParser graves = do whiteEndWhiteParser
                        whatHappend <-  string "on" <|> string "after" <|> string "every" <|> string "Bucket" <|> string "Citizen" <|> string "Farmer" <|> string "Dog"
                        whiteEndWhiteParser
                        case whatHappend of
                               "on" -> onLaneParser graves
                               "after" -> afterParser graves
                               "every" -> everySpawnParser graves
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
mapParser = do lines <- traverse maplineParser [0..5] -- Parse 6 times and give argument
               return $ foldl mergeMaps (Map [] [] []) lines

mergeMaps :: Map -> Map -> Map
mergeMaps x y = Map (wall x ++ wall y) (homes x ++ homes y) (graves x ++ graves y)

maplineParser :: Float -> Parser Map
maplineParser y = do cells <- count 9 cell
                     endlineParser
                     return  $ cellsToMap y cells (Map [] [] [])
 where cell = hex <|> token 'X' <|> grave
       hex = spot (\x -> isHexDigit x && (isLower x || isDigit x))
       grave = spot isUpper

cellsToMap :: Float -> String -> Map -> Map
cellsToMap _ [] map = map
cellsToMap y [x] map = map2 x (8,y) map
cellsToMap y s@(x:xs) map = cellsToMap y xs (map2 x ( fromIntegral $ 9 - length s, y) map)

map2 :: Char -> (Float, Float) -> Map -> Map
map2 x c mapp | isHexDigit x && (isLower x || isDigit x) = mapp { wall = wall mapp ++ newwalls x}
              | x == 'X' || x == 'x' = mapp { homes = c:homes mapp}
              | isUpper x = mapp { graves = (c,x):graves mapp}
              | otherwise = mapp
 where newwalls g = map (\y -> (c,add c y)) (binaryToDirection directions g)
       add (x,y) (x',y') = (x+x',y+y')

binaryToDirection :: [Coordinate] -> Char -> [Coordinate]
binaryToDirection xs = map fst . filter ((==) '1'. snd) . zip xs . hexToBin
 where hexToBin g = case readHex [g] of
                           (x,""):_ -> printf "%04b" (x::Int)
                           _       -> "0000"

-- | Parse a level
levelParser :: Parser Level
levelParser = do title <- titleParser
                 diff <- difficultyParser
                 endlineParser
                 seeds <- seedsParser
                 endlineParser
                 levelmap <- mapParser
                 phases <- some $ phaseParser (graves levelmap)
                 return $ Level title diff seeds [] [] phases 10 Nothing levelmap