module Data.Eulalie.Char where

import Prelude

import Data.String as String

import Data.Eulalie.Char.Predicates as Pred
import Data.Eulalie.Parser (Parser(), expected, sat, maybe)

many :: Parser Char -> Parser String
many parser = maybe $ many1 parser

many1 :: Parser Char -> Parser String
many1 parser = do
  head <- parser
  tail <- many parser
  return (String.fromChar head ++ tail)

char :: Char -> Parser Char
char c = expected (sat \i -> i == c) $ "\"" ++ String.fromChar c ++ "\""

notChar :: Char -> Parser Char
notChar c = expected (sat \i -> i /= c) $
            "anything but \"" ++ String.fromChar c ++ "\""

digit :: Parser Char
digit = expected (sat Pred.isDigit) "a digit"

space :: Parser Char
space = expected (sat Pred.isSpace) "whitespace"

alphanum :: Parser Char
alphanum = expected (sat Pred.isAlphanum) "a word character"

letter :: Parser Char
letter = expected (sat Pred.isLetter) "a letter"

upper :: Parser Char
upper = expected (sat Pred.isUpper) "an upper case letter"

lower :: Parser Char
lower = expected (sat Pred.isLower) "a lower case letter"

notDigit :: Parser Char
notDigit = expected (sat (not Pred.isDigit)) "a non-digit"

notSpace :: Parser Char
notSpace = expected (sat (not Pred.isSpace)) "a non-whitespace character"

notAlphanum :: Parser Char
notAlphanum = expected (sat (not Pred.isAlphanum)) "a non-word character"

notLetter :: Parser Char
notLetter = expected (sat (not Pred.isLetter)) "a non-letter"

notUpper :: Parser Char
notUpper = expected (sat (not Pred.isUpper)) "anything but an upper case letter"

notLower :: Parser Char
notLower = expected (sat (not Pred.isLower)) "anything but a lower case letter"
