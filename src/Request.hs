module Request (
  gzipRequest,
  gzipSimpleHTTP
) where

-- from
-- http://stackoverflow.com/questions/3784765/how-to-make-haskells-network-browser-do-gzip-compression

import Codec.Compression.GZip (decompress)
import Control.Arrow (second)
import Control.Monad (liftM)
import Control.Monad.Extra (discard)
import qualified Data.ByteString.Lazy as B
import Data.ByteString.Lazy.Char8 (unpack)
import Data.Functor.Monadic ((>=$>))
import Network.Browser (BrowserAction, browse, request, setOutHandler, defaultGETRequest_)
import Network.HTTP (Response, rspBody)
import Network.HTTP.Headers (HeaderName (..), findHeader, replaceHeader)
import Network.TCP (HandleStream)
import Network.URI (URI, parseURI)

type RequestAction = BrowserAction (HandleStream B.ByteString) (URI, Response B.ByteString)

gzipRequest :: URI -> RequestAction
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
        isGz = maybe False (== "gzip") . findHeader HdrContentEncoding

responseToString :: (URI, Response B.ByteString) -> String
responseToString = unpack . rspBody . snd

silenceReq :: RequestAction -> RequestAction
silenceReq = (setOutHandler discard *>)

gzipSimpleHTTP :: String -> IO (Maybe String)
gzipSimpleHTTP = sequence . (parseURI >=$> gzipRequest >=$> silenceReq >=$> browse >=$> fmap responseToString)
