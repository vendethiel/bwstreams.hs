module Main where

import Data.Char (toLower)
import Data.Foldable (traverse_, find)
import Data.Maybe (fromMaybe)

import Request (gzipSimpleHTTP)
import TLParser (getGameStreams)

formatName :: (String, String) -> String
formatName (en, kr) = en ++ " (" ++ kr ++ ")"

readOneOf :: [String] -> IO String -> IO String
readOneOf xs get = get >>= \val -> fromMaybe
    (putStrLn "Invalid pick" >> readOneOf xs get)
    (return <$> find (== val) xs)

lowerStr :: String -> String
lowerStr = fmap toLower

pickStream :: [(String, String)] -> IO ()
pickStream streams = do
  putStrLn "Pick a stream"
  traverse_ (putStrLn . formatName) streams
  choice <- readOneOf (lowerStr <$> fst <$> streams) (lowerStr <$> getLine)
  -- TODO find a way not to call find again, without making readOneOf aware of the tuple layout...
  --let krName = fromJust $ snd <$>
  putStrLn $ "Your choice is " ++ choice

process :: String -> IO ()
process = either (const $ putStrLn "Unable to parse TeamLiquid XML") pickStream . getGameStreams "BW"

main :: IO ()
main =
  gzipSimpleHTTP "http://www.teamliquid.net/video/streams/?xml=1&filter=live"
  >>= maybe (putStrLn "Unable to query teamliquid") process
