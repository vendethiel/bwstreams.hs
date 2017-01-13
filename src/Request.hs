module Request (
  gzipRequest,
  gzipSimpleHTTP
) where

-- from
-- http://stackoverflow.com/questions/3784765/how-to-make-haskells-network-browser-do-gzip-compression

import Codec.Compression.GZip (decompress)
import Control.Arrow (second)
import Control.Monad (liftM, (>=>))
import qualified Data.ByteString.Lazy as B
import Data.ByteString.Lazy.Char8 (unpack)
import Data.Functor.Monadic ((>=$>))
import Network.Browser (BrowserAction (..), browse, request, defaultGETRequest_)
import Network.HTTP (Request, Response, getRequest, getResponseBody, rspBody)
import Network.HTTP.Headers (HeaderName (..), findHeader, replaceHeader)
import Network.TCP (HStream, HandleStream)
import Network.URI (URI, parseURI)

gzipRequest :: URI -> BrowserAction (HandleStream B.ByteString) (URI, Response B.ByteString)
gzipRequest
  = liftM (second unzipIfNeeded)
  . request
  . replaceHeader HdrAcceptEncoding "gzip"
  . defaultGETRequest_
  where
    unzipIfNeeded rsp
      | isGz rsp  = rsp { rspBody = decompress $ rspBody rsp }
      | otherwise = rsp
      where
        isGz rsp = maybe False (== "gzip") $ findHeader HdrContentEncoding rsp

actionToString :: (URI, Response B.ByteString) -> String
actionToString = unpack . rspBody . snd

gzipSimpleHTTP :: String -> IO (Maybe String)
gzipSimpleHTTP = sequence . (parseURI >=$> gzipRequest >=$> browse >=$> fmap actionToString)
