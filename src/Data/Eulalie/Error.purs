module Data.Eulalie.Error where

import Prelude
import Data.Set as Set
import Data.String as String
import Data.Array (fromFoldable)
import Data.Eulalie.Stream (Stream(..), atEnd)
import Data.Set (Set)
import Data.String (fromCharArray)

type ParseError i =
  { input :: Stream i
  , expected :: Set String
  , fatal :: Boolean
  }

withExpected :: forall i. ParseError i -> Set String -> ParseError i
withExpected err expected = err { expected = expected }

escalate :: forall i. ParseError i-> ParseError i
escalate err = err { fatal = true }

extend :: forall i. ParseError i -> ParseError i -> ParseError i
extend e1@{ expected: x1, input: Stream s1 }
  e2@{ expected: x2, input: Stream s2 } =
  if s1.cursor < s2.cursor then e2
  else if s1.cursor > s2.cursor then e1
       else e1 { expected = Set.union x1 x2 }

print :: ParseError Char -> String
print { input: input@Stream { buffer, cursor }, expected } =
  "Expected " <> exp expected <> ", saw "
  <> (if atEnd input then "EOF" else quote $ String.take 6 $ String.drop cursor buf) <> "\n\"" <> buf <> "\""
  where buf = fromCharArray buffer
        quote s = "\"" <> s <> "\""
        exp xs = String.joinWith " or " $ fromFoldable xs
