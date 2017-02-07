module Main where

import Data.Char (toLower)
import Data.Foldable (traverse_, find)
import Data.Maybe (fromMaybe)

import Request (gzipSimpleHTTP)
import TLParser (getGameStreams, Stream, enName, krName)
import LiveStream (launchLiveStream)

formatName :: Stream -> String
formatName stream = enName stream ++ " (" ++ krName stream ++ ")"

readOneOf :: (a -> String) -> [a] -> IO String -> IO a
readOneOf extractor xs get = get >>= \val -> fromMaybe
    (putStrLn "Invalid pick" >> readOneOf extractor xs get)
    (return <$> find ((== val) . extractor) xs)

lowerStr :: String -> String
lowerStr = fmap toLower

pickStream :: [Stream] -> IO ()
pickStream streams = do
  putStrLn "Pick a stream"
  traverse_ (putStrLn . formatName) streams
  choice <- readOneOf (lowerStr . enName) streams (lowerStr <$> getLine)
  putStrLn $ "Your choice is " ++ enName choice ++ ". Launching..."
  launchLiveStream $ "afreeca.com/" ++ krName choice

process :: String -> IO ()
process = either (const $ putStrLn "Unable to parse TeamLiquid XML") pickStream . getGameStreams "BW"

main :: IO ()
main =
  gzipSimpleHTTP "http://www.teamliquid.net/video/streams/?xml=1&filter=live"
  >>= maybe (putStrLn "Unable to query teamliquid") process
