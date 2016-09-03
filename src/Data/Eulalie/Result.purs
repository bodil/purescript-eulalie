module Data.Eulalie.Result where

import Data.Set as Set
import Data.Set (Set())

import Data.Eulalie.Error (ParseError())
import Data.Eulalie.Stream (Stream())
import Data.Eulalie.Success (ParseSuccess())

data ParseResult i o = Success (ParseSuccess i o)
                     | Error (ParseError i)

success :: forall i o. o -> Stream i -> Stream i -> Array i -> ParseResult i o
success value next start matched = Success { value, next, start, matched }

error :: forall i o. Stream i -> ParseResult i o
error input = Error { input, expected: Set.empty, fatal: false }

error' :: forall i o. Stream i -> Set String -> Boolean -> ParseResult i o
error' input expected fatal = Error { input, expected, fatal }
