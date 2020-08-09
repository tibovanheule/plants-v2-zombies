module Main where
import System.Environment
import System.Exit
import Types
import System.IO
import Data.Either
import System.Directory
import System.FilePath
import Control.Applicative
import Control.Monad
import LevelParser
import Parser
import SetupParser
import Game

main :: IO ()
main = do args <- getArgs
          doesFileExist  (head args) >>= flip unless (die "level file not found")
          doesFileExist  (last args) >>= flip unless (die "defence file not found")
          contentLevel <- readFile (head args)
          let level = getLevel contentLevel
          case level of
               (Left err) -> print "Error parsing level" >> hPutStrLn stderr err >> exitFailure
               _ -> print "Successfully parsed level"
          contentDefence <- readFile (last args)
          let defence = getDefence contentDefence
          case defence of
                (Left err) -> print "Error parsing Defence" >> hPutStrLn stderr err >> exitWith (ExitFailure 2 )
                _ -> print "Successfully parsed defence"
          let leveldef = (head $ rights [level]) {plants=head $ rights [defence]}
          let game = createGame leveldef
          runTest game

-- persoonlijk vind ik deze code te kort voor in een aparte module te steken
-- | run a game in Test modus, simulates time ticks
runTest :: World -> IO()
runTest game = case getState game of
                    Ongoing -> tick game
                    Menu -> die "ERROR, state corrupted"
                    Lost -> putStr "Loss after " >> putStr ( show (getTime game)) >> putStr " seconds" >> exitSuccess
                    Won -> putStr "Victory at " >> putStr (show (getTime game)) >> exitSuccess

-- | Give the world a time tick, then let runtest decide if a tick is again needed
tick :: World -> IO()
tick w@(World _ (Just _) _ _ _) = runTest $ changeWorld w
tick (World _ Nothing _ _ _) = die "ERROR, no level"

-- | Call parser en parse defence file contents using setupparser
getDefence :: String -> Either Error [Plant]
getDefence = parseStatement setupParser

-- | Call parser en parse level file contents using levelparser
getLevel :: String -> Either Error Level
getLevel =  parseStatement levelParser