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
--readOneOf :: [a] -> IO a -> IO a
--readOneOf xs get = do
--  val <- get
--  if get `elem` xs
--    then return get
--    else putStrLn "Invalid choice" >> readOneOf xs f

lowerStr :: String -> String
lowerStr = fmap toLower

pickStream :: [(String, String)] -> IO ()
pickStream streams = do
  putStrLn "Pick a stream"
  traverse_ (putStrLn . formatName) streams
  choice <- readOneOf (lowerStr <$> fst <$> streams) (lowerStr <$> getLine)
  putStrLn $ "Your choice is " ++ choice

process :: String -> IO ()
process = either (const $ putStrLn "Unable to parse TeamLiquid XML") pickStream . getGameStreams "BW"

main :: IO ()
main =
  gzipSimpleHTTP "http://www.teamliquid.net/video/streams/?xml=1&filter=live"
  >>= maybe (putStrLn "Unable to query teamliquid") process
