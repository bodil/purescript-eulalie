module Data.Eulalie.Stream where

import Prelude
import Data.Array (length, (!!))
import Data.Maybe (Maybe(..))

newtype Stream a = Stream { buffer :: Array a, cursor :: Int }

stream :: forall a. Array a -> Stream a
stream s = Stream { buffer: s, cursor: 0 }

get :: forall a. Stream a -> Maybe a
get (Stream {buffer, cursor}) = buffer !! cursor

atEnd :: forall a. Stream a -> Boolean
atEnd (Stream {buffer, cursor}) = cursor >= length buffer

next :: forall a. Stream a -> Maybe (Stream a)
next s@(Stream {buffer, cursor}) =
  if atEnd s then Just $ Stream { buffer, cursor: (cursor + 1) } else Nothing

getAndNext :: forall a. Stream a -> Maybe { value :: a, next :: Stream a }
getAndNext (Stream {buffer, cursor}) = case buffer !! cursor of
  Just c -> Just $ { value: c, next: Stream { buffer, cursor: cursor + 1 } }
  Nothing -> Nothing

instance eqStream :: (Eq a) => Eq (Stream a) where
  eq (Stream {buffer: b1, cursor: c1}) (Stream {buffer: b2, cursor: c2}) =
    b1 == b2 && c1 == c2

instance showStream :: (Show a) => Show (Stream a) where
  show (Stream {buffer, cursor}) = "Stream " <> show cursor <> " " <> show buffer
