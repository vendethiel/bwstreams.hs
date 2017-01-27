module Main where

import Data.Foldable (traverse_)

import Request (gzipSimpleHTTP)
import TLParser (getGameStreams)

pickStream :: [] String -> IO ()
pickStream streams = do
  putStrLn "Pick a stream"
  traverse_ putStrLn $ streams
  choice <- getLine
  putStrLn $ "Your choice is " ++ choice

process :: String -> IO ()
process = either (const $ putStrLn "Unable to parse TeamLiquid XML") pickStream . getGameStreams "BW"

main :: IO ()
main =
  gzipSimpleHTTP "http://www.teamliquid.net/video/streams/?xml=1&filter=live"
  >>= maybe (putStrLn "Unable to query teamliquid") process
