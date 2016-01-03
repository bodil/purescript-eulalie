module Data.Eulalie.Char.Predicates where

import Prelude

import Data.Char as Char
import Data.String as Str
import Data.String.Regex as Re

notP :: forall a. (a -> Boolean) -> (a -> Boolean)
notP f = \v -> not (f v)

isDigit :: Char -> Boolean
isDigit c = Re.test (Re.regex "^\\d$" Re.noFlags) (Str.fromChar c)

isSpace :: Char -> Boolean
isSpace c = Re.test (Re.regex "^\\s$" Re.noFlags) (Str.fromChar c)

isAlphanum :: Char -> Boolean
isAlphanum c = Re.test (Re.regex "^\\w$" Re.noFlags) (Str.fromChar c)

isLetter :: Char -> Boolean
isLetter c = Re.test (Re.regex "^\\[a-zA-Z]$" Re.noFlags) (Str.fromChar c)

isUpper :: Char -> Boolean
isUpper c = isLetter c && c == Char.toUpper c

isLower :: Char -> Boolean
isLower c = isLetter c && c == Char.toLower c

unless :: (Char -> Boolean) -> (Char -> Boolean) -> (Char -> Boolean)
unless a b = \v -> not (b v) && a v

and :: (Char -> Boolean) -> (Char -> Boolean) -> (Char -> Boolean)
and a b = \v -> a v && b v

or :: (Char -> Boolean) -> (Char -> Boolean) -> (Char -> Boolean)
or a b = \v -> a v || b v
