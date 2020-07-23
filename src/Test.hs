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
