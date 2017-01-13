module Main where

import Control.Monad (void)
import Network.HTTP (getRequest, simpleHTTP, getResponseBody)
import Request (gzipSimpleHTTP)

main :: IO ()
main = do
  body <- gzipSimpleHTTP "http://www.teamliquid.net/video/streams/?xml=1&filter=live"
  mapM_ putStrLn body
