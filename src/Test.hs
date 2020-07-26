module Main where
import System.Environment
import System.Exit
import Game
import Types
import System.IO
import Data.Either
import System.Directory
import System.FilePath
import Control.Applicative
import Control.Monad

main :: IO ()
main = do args <- getArgs
          doesFileExist  (head args) >>= \x -> when (not x) (die "level file not found")
          doesFileExist  (last args) >>= \x -> when (not x) (die "defence file not found")
          contentLevel <- readFile (head args)
          let level = getLevel contentLevel
          case level of
               (Left err) -> print "Error parsing level" >> hPutStrLn stderr err >> exitFailure
               _ -> print "Successfully parsed level"
          contentDefence <- readFile (last args)
          let defence = getDefence contentDefence
          case defence of
                (Left err) -> print "Error parsing Defence" >> hPutStrLn stderr err  >> exitWith (ExitFailure 2 )
                _ -> print "Successfully parsed defence"
          let leveldef = (head $ rights [level]) {plants=head $ rights [defence]}
          let game = createGame leveldef
          runTest game

-- persoonlijk vind ik deze code te kort voor in een aparte module te steken
-- | run a game in Test modus, simulates time ticks
runTest :: World -> IO()
runTest game = case getState game of
                    Ongoing -> print game >> tick game
                    Menu -> die "ERROR, state corrupted"
                    Lost -> putStr "Loss after " >> putStr ( show (getTime game)) >> putStr " seconds" >> exitSuccess
                    Won -> putStr "Victory at " >> putStr (show (getTime game)) >> exitSuccess

-- | Give the world a time tick, then let runtest decide if a tick is again needed
tick :: World -> IO()
tick (World time (Just l) _ _ e _) = print time >> (runTest $ changeWorld time e l)
tick (World _ Nothing _ _ _ _) = die "ERROR, no level"