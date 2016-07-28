module Data.Eulalie.Result where

import Data.Set as Set
import Data.Set (Set())

import Data.Eulalie.Error (ParseError())
import Data.Eulalie.Stream (Stream())
import Data.Eulalie.Success (ParseSuccess())

data ParseResult a = Success (ParseSuccess a)
                   | Error ParseError

success :: forall a. a -> Stream -> Stream -> String -> ParseResult a
success value next start matched = Success { value, next, start, matched }

error :: forall a. Stream -> ParseResult a
error input = Error { input, expected: Set.empty, fatal: false }

error' :: forall a. Stream -> Set String -> Boolean -> ParseResult a
error' input expected fatal = Error { input, expected, fatal }
