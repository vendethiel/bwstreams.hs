module TLParser (
  getGameStreams
) where

import Control.Exception (SomeException) -- fck you xml lib
import Control.Monad ((>=>))
import Data.ByteString.Lazy.UTF8 (fromString)
import Data.Functor.Monadic ((<$=<))
import qualified Data.Text as T
import Text.XML (parseLBS, def)
import Text.XML.Cursor (Cursor, fromDocument, element, content, attributeIs, child, (&//), ($//))

cursorFor :: String -> Either SomeException Cursor
cursorFor = fromDocument <$=< (parseLBS def . fromString)

selector :: String -> Cursor -> [T.Text]
selector game cursor = cursor $// element "stream"
                              >=> attributeIs "type" (T.pack game)
                              >=> child
                              >=> element "channel"
                              >=> attributeIs "type" "Afreeca"
                              &// content

getGameStreams :: String -> String -> Either SomeException [String]
getGameStreams game xml = (fmap T.unpack) <$> selector game <$> cursorFor xml

