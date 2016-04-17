module Data.Eulalie.Char where

import Prelude

import Data.Foldable (any)
import Data.String as String

import Data.Eulalie.Char.Predicates as Pred
import Data.Eulalie.Parser (Parser(), expected, sat, maybe)

-- |Takes a `Parser Char` and matches it zero or more times, returning
-- |a `String` of what was matched.
many :: Parser Char -> Parser String
many parser = maybe $ many1 parser

-- |Takes a `Parser Char` and matches it one or more times, returning
-- |a `String` of what was matched.
many1 :: Parser Char -> Parser String
many1 parser = do
  head <- parser
  tail <- many parser
  return (String.fromChar head ++ tail)

-- |The `char` parser constructor returns a parser which matches only the
-- |specified single character.
char :: Char -> Parser Char
char c = expected (sat \i -> i == c) $ "\"" ++ String.fromChar c ++ "\""

-- |The `notChar` parser constructor makes a parser which will match any
-- |single character other than the one provided.
notChar :: Char -> Parser Char
notChar c = expected (sat \i -> i /= c) $
            "anything but \"" ++ String.fromChar c ++ "\""

-- |Matches any one character from the provided string.
oneOf :: String -> Parser Char
oneOf s = expected (sat \i -> any (\c -> i == c) cs) $ "one of \"" ++ s ++ "\""
  where cs = String.toCharArray s

-- |Matches a single digit.
digit :: Parser Char
digit = expected (sat Pred.isDigit) "a digit"

-- |Matches a single whitespace character.
space :: Parser Char
space = expected (sat Pred.isSpace) "whitespace"

-- |Matches a single letter, digit or underscore character.
alphanum :: Parser Char
alphanum = expected (sat Pred.isAlphanum) "a word character"

-- |Matches a single ASCII letter.
letter :: Parser Char
letter = expected (sat Pred.isLetter) "a letter"

-- |Matches a single upper case ASCII letter.
upper :: Parser Char
upper = expected (sat Pred.isUpper) "an upper case letter"

-- |Matches a single lower case ASCII letter.
lower :: Parser Char
lower = expected (sat Pred.isLower) "a lower case letter"

-- |Matches a single character which isn't a digit.
notDigit :: Parser Char
notDigit = expected (sat (not Pred.isDigit)) "a non-digit"

-- |Matches a single character which isn't whitespace.
notSpace :: Parser Char
notSpace = expected (sat (not Pred.isSpace)) "a non-whitespace character"

-- |Matches a single character which isn't a letter, digit or underscore.
notAlphanum :: Parser Char
notAlphanum = expected (sat (not Pred.isAlphanum)) "a non-word character"

-- |Matches a single character which isn't an ASCII letter.
notLetter :: Parser Char
notLetter = expected (sat (not Pred.isLetter)) "a non-letter"

-- |Matches a single character which isn't an upper case ASCII letter.
notUpper :: Parser Char
notUpper = expected (sat (not Pred.isUpper)) "anything but an upper case letter"

-- |Matches a single character which isn't a lower case ASCII letter.
notLower :: Parser Char
notLower = expected (sat (not Pred.isLower)) "anything but a lower case letter"
