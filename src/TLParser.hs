{-# LANGUAGE OverloadedStrings #-}

module TLParser (
  Stream, enName, krName,
  getGameStreams
) where

import Control.Exception (SomeException) -- fck you xml lib
import Control.Monad ((>=>))
import Data.ByteString.Lazy.UTF8 (fromString)
import Data.Functor.Monadic ((<$=<), (>$>))
import qualified Data.Text as T
import Text.XML (parseLBS, def)
-- note: GHC says Cursor is redundent on the following line. Lies.
import Text.XML.Cursor (Cursor, fromDocument, element, content, attributeIs, attribute, child, node, ($//))

data Stream = Stream { enName :: String, krName :: String }

cursorFor :: String -> Either SomeException Cursor
cursorFor = fromDocument <$=< (parseLBS def . fromString)

selector :: String -> Cursor -> [Stream]
selector game cursor = zipWith Stream (T.unpack <$> titles) (T.unpack <$> contents)
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

getGameStreams :: String -> String -> Either SomeException [Stream]
getGameStreams game xml = selector game <$> cursorFor xml

