module Parser (Parser, token, string, some, many, optional, empty, spot, orParser, geefError, parseStatement, (<|>)) where

import           Control.Applicative (Alternative (..), many, optional, some,
                                      (<|>))
import           Control.Monad       (MonadPlus (..), ap, liftM, guard)
import           Data.Either         (isLeft, isRight)
import Types
-- Types

newtype Parser a = Parser (String -> [(Either Error a, String)])

-- Use the parser to parse a string
parse :: Parser a -> String -> [(Either Error a, String)]
parse (Parser p) = p


-- Parse a string a with a gegeven parser en geef either terug
parseStatement :: Parser a -> String -> Either Error a
parseStatement parser a = case parse parser a of
    -- success, geen rest meer
    [(Right x,"")]  -> Right x
    -- er is een error message gegeven door de parser zelf
    [(Left x, _)]   -> Left x
    -- Het is niet volledig geparsed :(
    [(_,rest)]      -> Left $ "near:" ++ take 25 rest
    -- grandioos gefaald
    []              -> Left "Failed to parse"
    -- meerdere mogelijkheden
    (_, _):(_, _):_ -> Left "Ambiguous parse"

-- Parse one character
char :: Parser Char
char = Parser f
  where  f [] = []
         f (c:s) = [(Right c,s)]

-- Parse a character satisfying a predicate (e.g., isDigit)
spot :: (Char -> Bool) -> Parser Char
spot p = do { c <- char; guard (p c); return c }

-- Match a given character
token :: Char -> Parser Char
token c = spot (== c)

-- Zit er een string in?
string :: String -> Parser String
string s = do
    mapM_ token s
    return s
    
geefError :: Error -> Parser a
geefError msg = Parser (\cs -> [(Left msg, cs)])               

-- Vindt de eerste parser die werkt maar, hou een volgorde aan.
orParser :: [Parser a] -> Parser a
orParser = foldl1 (<|>)

instance Monad Parser where
    return a = Parser (\cs -> [(Right a,cs)])
    p >>= f  = Parser (\cs ->
                        let firstParsed = parse p cs -- List of (a, cs')
                            succeeded   = (\(Right x, cs'') -> (x, cs'')) <$> filter (isRight . fst) firstParsed
                            failed      = (\(Left x,  cs'') -> (x, cs'')) <$> filter (isLeft . fst) firstParsed
                        in case succeeded of
                            [] -> case failed of
                                []             -> []
                                ((msg, cs'):_) -> [(Left msg, cs')]
                            _  -> concat [parse (f a) cs' | (a, cs') <- succeeded]
                      )

instance Functor Parser where
    fmap = liftM

instance Applicative Parser where
    pure  = return
    (<*>) = ap

instance MonadPlus Parser where
    mzero = Parser (const [])
    m1 `mplus` m2 = Parser (\s -> parse m1 s ++ parse m2 s)

bestCase :: [(Either a b, c)] -> [(Either a b, c)]
bestCase [] = []
bestCase ((Right v, cs) : _) = [(Right v, cs)]
bestCase (firsterror@(Left _, _) : xs) = case bestCase xs of
    []            -> [firsterror]
    [(Left _, _)] -> [firsterror]
    other         -> other

-- De alternatieve bepaalt wat er gebeurd als meerdere parsers zijn.
instance Alternative Parser where
    p1 <|> p2 = Parser $ \s -> bestCase $ parse (mplus p1 p2) s
    empty = mzero