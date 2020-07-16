module Main where
import System.Environment
import System.Exit
import Game
import Types
import System.IO
import Data.Either

main :: IO ()
main = do args <- getArgs
          contentLevel <- readFile (head args)
          let level = getLevel contentLevel
          case level of
               (Left err) -> die ("Error parsing level" ++ err)
               _ -> print "Successfully parsed level"
          contentDefence <- readFile (last args)
          let defence = getDefence contentDefence
          case defence of
                (Left _) -> die "Error parsing Defence"
                _ -> print "Successfully parsed defence"
          let leveldef = (head $ rights [level]) {plants=head $ rights [defence]}
          print leveldef
          let game = createGame leveldef
          runTest game
