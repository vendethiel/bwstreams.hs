module TLParser (
  getGameStreams
) where

import Debug.Trace
import Control.Arrow ((&&&))
import Control.Exception (SomeException) -- fck you xml lib
import Control.Monad ((>=>), join)
import Data.Bifunctor (bimap)
import Data.ByteString.Lazy.UTF8 (fromString)
import Data.Functor.Monadic ((<$=<), (>$>))
import qualified Data.Text as T
import Text.XML (parseLBS, def)
import Text.XML.Cursor (Cursor, fromDocument, element, content, attributeIs, attribute, child, node, (&//), ($//))

cursorFor :: String -> Either SomeException Cursor
cursorFor = fromDocument <$=< (parseLBS def . fromString)

selector :: String -> Cursor -> [(T.Text, T.Text)]
selector game cursor = zip titles contents
  where
    nodes :: [Cursor]
    nodes = cursor $// element "stream"
           >=> attributeIs "type" (T.pack game)
           >=> child -- gives an error
           >=> element "channel"
           >=> attributeIs "type" "Afreeca"
    titles :: [T.Text] -- attribute returns a [T.Text] that we concat
    titles = T.concat <$> attribute "title" <$> nodes
    contents :: [T.Text] -- children of each node, get the content nodes, concat them, then concat the child's contents (we don't statically know that there's only one child)
    contents = nodes >$> child >$> fmap (T.concat . content) >$> T.concat

getGameStreams :: String -> String -> Either SomeException [(String, String)]
getGameStreams game xml = (fmap $ join bimap T.unpack) <$> selector game <$> cursorFor xml
                                -- join+bimap to map the tuple could use lenses, i.e. fmap (& both %~ T.unpack)

