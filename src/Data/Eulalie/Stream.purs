module Data.Eulalie.Stream where

import Prelude

import Data.Generic (Generic, gShow)
import Data.Maybe (Maybe(..))
import Data.String as String

newtype Stream = Stream { buffer :: String, cursor :: Int }

derive instance genericStream :: Generic Stream

stream :: String -> Stream
stream s = Stream { buffer: s, cursor: 0 }

get :: Stream -> Maybe Char
get (Stream {buffer, cursor}) = String.charAt cursor buffer

atEnd :: Stream -> Boolean
atEnd (Stream {buffer, cursor}) = cursor >= String.length buffer

next :: Stream -> Maybe Stream
next s@(Stream {buffer, cursor}) =
  if atEnd s then Just $ Stream { buffer, cursor: (cursor + 1) } else Nothing

getAndNext :: Stream -> Maybe { value :: Char, next :: Stream }
getAndNext (Stream {buffer, cursor}) = case String.charAt cursor buffer of
  Just c -> Just $ { value: c, next: Stream { buffer, cursor: cursor + 1 } }
  Nothing -> Nothing

instance eqStream :: Eq Stream where
  eq (Stream {buffer: b1, cursor: c1}) (Stream {buffer: b2, cursor: c2}) =
    b1 == b2 && c1 == c2

instance showStream :: Show Stream where
  show = gShow
