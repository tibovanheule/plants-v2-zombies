-- | Setup parser is a parser for setup files given in the main
module SetupParser where
import Parser
import Types
import Control.Monad (void)

setupParser :: Parser [Plant]
setupParser = many lineParser

lineParser :: Parser Plant
lineParser = do
                token '('
                x <- digitParser
                token ','
                parseWhiteSpace
                y <- digitParser
                token ','
                parseWhiteSpace
                seed <- seedParser
                life <- case seed of
                             Walnut -> return 50
                             _ -> return 1
                token ')'
                endlineParser
                return (Plant seed life (read [x],read [y]) 0 [])

seedParser :: Parser PlantType
seedParser  = do x <- string "Sunflower" <|> string "Peashooter" <|> string "Walnut"
                 return (read x ::PlantType)