module Data.Eulalie.Error where

import Prelude

import Data.List as List
import Data.Set (Set())
import Data.Set as Set
import Data.String as String

import Data.Eulalie.Stream (Stream(..), atEnd)

type ParseError = { input :: Stream, expected :: Set String,
                    fatal :: Boolean }

withExpected :: ParseError -> Set String -> ParseError
withExpected err expected = err { expected = expected }

escalate :: ParseError -> ParseError
escalate err = err { fatal = true }

extend :: ParseError -> ParseError -> ParseError
extend e1@{ expected: x1, input: Stream s1 }
  e2@{ expected: x2, input: Stream s2 } =
  if s1.cursor < s2.cursor then e2
  else if s1.cursor > s2.cursor then e1
       else e1 { expected = Set.union x1 x2 }

print :: ParseError -> String
print { input: input@Stream { buffer, cursor }, expected } =
  "Expected " ++ exp expected ++ ", saw "
  ++ (if atEnd input then "EOF" else quote $ String.take 6 $ String.drop cursor buffer) ++ "\n\"" ++ buffer ++ "\""
  where quote s = "\"" ++ s ++ "\""
        exp xs = String.joinWith " or " $ List.fromList $ Set.toList xs
