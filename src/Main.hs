-- | Main graphic entry
module Main where
import System.Directory
import Types
import System.Environment
import System.IO
import Data.Either
import System.Exit
import Control.Applicative
import Control.Monad
import System.FilePath
import Graphical
import LevelParser
import Parser

-- Ongeldige bestanden mag je in deze modus gewoon negeren.
main :: IO ()
main = do
          args <- getArgs
          --- THESE ARE DEBUGGING OPTIONS FOR more info when running the program (As described in README)
          let [debug,time, events, exit] = map (\x -> elem x args) ["debug","time", "events", "exit"]
          -- Haal programma argumenten op.
          fp <- head <$> getArgs
          -- Bestaat de directory.
          doesDirectoryExist fp >>= \x -> when (not x) (die "directory not found")
          -- Zoek in de map alle level files.
          contents <- getDirFilesPaths fp
          -- DEBUG print
          when debug (print contents)
          -- Parse all level files
          parsedFiles <-  readLevelFiles contents
          -- DEBUG print
          when debug (print $ rights parsedFiles)
          -- Are there any successfully parsed files?
          case parsedFiles of
                 [] -> die "No readably files"
                 _ ->  when debug (print "Levels loaded")
          -- DEBUG we only want the parsing info
          when exit (exitSuccess)
          -- Start GUI
          graphic time events $ createPossibleGame (rights parsedFiles)

-- | Get all FilePaths of the files in a directory.
getDirFilesPaths :: FilePath -> IO [FilePath]
getDirFilesPaths fp = map ((fp ++ "/") ++) . filter check  <$> getDirectoryContents fp
                      where check x = isfile x && islevelfile x
                            isfile x = notElem x [".",".."]
                            islevelfile x = drop (length x - 9) x  == ".level.in"

-- | Convert array of filepaths to array of parsed levels.
readLevelFiles :: [FilePath] -> IO [Either Error Level]
readLevelFiles = mapM readLevelFile

-- | Parse filepath to parsed level.
readLevelFile :: FilePath -> IO (Either Error Level)
readLevelFile file = readFile file >>= return . getLevel

-- | Call parser en parse level file contents using levelparser
getLevel :: String -> Either Error Level
getLevel =  parseStatement levelParser