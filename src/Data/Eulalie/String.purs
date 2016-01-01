module Data.Eulalie.String where

import Prelude

import Control.Alt ((<|>))
import Control.Apply ((*>))
import Data.Foldable (fold)
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Monoid (Monoid)
import Data.String as String
import Global (readFloat, isNaN)

import Data.Eulalie.Char as Char
import Data.Eulalie.Parser (Parser(), expected, maybe, fail, item)

many :: Parser String -> Parser String
many parser = maybe $ many1 parser

many1 :: Parser String -> Parser String
many1 parser = do
  head <- parser
  tail <- many parser
  return (head ++ tail)

fromChar :: Parser Char -> Parser String
fromChar parser = String.fromChar <$> parser

char :: Char -> Parser String
char = fromChar <<< Char.char

notChar :: Char -> Parser String
notChar = fromChar <<< Char.notChar

string :: String -> Parser String
string s = expected (string' s) $ "\"" ++ s ++ "\""
  where string' :: String -> Parser String
        string' s = case String.charAt 0 s of
          Nothing -> return ""
          Just c -> do
            Char.char c
            string' $ String.drop 1 s
            return s

spaces :: Parser String
spaces = Char.many Char.space

spaces1 :: Parser String
spaces1 = expected (Char.many1 Char.space) "whitespace"

notSpaces :: Parser String
notSpaces = Char.many Char.notSpace

notSpaces1 :: Parser String
notSpaces1 = expected (Char.many1 Char.notSpace)
             "one or more non-whitespace characters"

int :: Parser Int
int = expected int' "an integer"
  where int' = do
          r <- fold [ maybe (char '-'), Char.many1 Char.digit ]
          case Int.fromString r of
            Just i -> return i
            _ -> fail

float :: Parser Number
float = expected float' "a number"
  where float' = do
          r <- fold [ maybe (char '-'),
                         Char.many Char.digit,
                         maybe $ fold [ char '.', Char.many1 Char.digit ]
                      ]
          case readFloat r of
            n | isNaN n -> fail
            n -> return n

quotedString :: Parser String
quotedString = expected qs "a quoted string"
  where qs = do
          char '"'
          s <- many $ (char '\\' *> fromChar item) <|> notChar '"'
          char '"'
          return s
