-- | Main graphic entry
module Main where
import System.Directory
import Types
import Game
import System.Environment
import System.IO
import Data.Either
import System.Exit
import Control.Applicative
import Control.Monad
import System.FilePath
import Graphical

-- Ongeldige bestanden mag je in deze modus gewoon negeren.
main :: IO ()
main = do
          args <- getArgs
          let debug = "debug" `elem` args
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
          when debug (print parsedFiles)
          -- Are there any successfully parsed files?
          case parsedFiles of
                 [] -> die "No readably files"
                 _ ->  when debug (print "Levels loaded")
          graphic $ createPossibleGame (rights parsedFiles)

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
readLevelFile file = readFile file >>= \x -> return (getLevel x)