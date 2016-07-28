module Data.Eulalie.Char.Predicates where

import Prelude
import Data.Char as Char
import Data.String as Str
import Data.String.Regex as Re
import Data.Either (fromRight)
import Partial.Unsafe (unsafePartial)

notP :: forall a. (a -> Boolean) -> (a -> Boolean)
notP f = \v -> not (f v)

digitRe :: Re.Regex
digitRe = unsafePartial $ fromRight $ Re.regex "^\\d$" Re.noFlags

spaceRe :: Re.Regex
spaceRe = unsafePartial $ fromRight $ Re.regex "^\\s$" Re.noFlags

alphanumRe :: Re.Regex
alphanumRe = unsafePartial $ fromRight $ Re.regex "^\\w$" Re.noFlags

letterRe :: Re.Regex
letterRe = unsafePartial $ fromRight $ Re.regex "^\\w$" Re.noFlags

isDigit :: Char -> Boolean
isDigit c = Re.test digitRe (Str.singleton c)

isSpace :: Char -> Boolean
isSpace c = Re.test spaceRe (Str.singleton c)

isAlphanum :: Char -> Boolean
isAlphanum c = Re.test alphanumRe (Str.singleton c)

isLetter :: Char -> Boolean
isLetter c = Re.test letterRe (Str.singleton c)

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
