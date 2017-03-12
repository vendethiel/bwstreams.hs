module LiveStream (
  launchLiveStream
) where

import Control.Monad.Extra (skip)
import Data.Maybe (fromMaybe)
import System.Environment (getExecutablePath)
import System.Info (os)
import System.FilePath.Posix (takeDirectory)
import System.Process (readProcess)

launchLiveStream :: String -> IO ()
launchLiveStream url = do
  execPath <- getExecutablePath
  player <- readFile (takeDirectory execPath ++ "/../path.txt")
  output <- readProcess "livestreamer" [
    "--quiet",
    "--loglevel=error",
    "--player=" ++ player,
    url
    ] ""
  putStrLn output
  getLine >> skip -- pause after program
