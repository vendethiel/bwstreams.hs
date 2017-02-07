module LiveStream (
  launchLiveStream
) where

import Data.Maybe (fromMaybe)
import System.Info (os)
import System.Process (readProcess)

launchLiveStream :: String -> IO ()
launchLiveStream url = fromMaybe (putStrLn $ "Operating System unrecognized: " ++ os) $ do
  path <- vlcPath
  return $ readProcess "livestreamer" [
    "--quiet", -- not strictly needed as we discard output
    "--loglevel=error",
    "--player=" ++ path ++ " --file-caching=5000 --network-caching=5000",
    url
    ] "" >>= putStrLn
  
vlcPath :: Maybe String
vlcPath = case os of
  "darwin" -> Just "/Applications/VLC.app/Contents/MacOS/VLC"
  "mingw"  -> Just "C:\Program Files\VideoLAN\VLC\vlc.exe" -- TODO "C:\Program Files (x86)\VideoLAN\VLC\vlc.exe"
