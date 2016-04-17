module Data.Eulalie.Parser where

import Prelude

import Control.Alt (class Alt, (<|>))
import Control.Alternative (class Alternative)
import Control.Apply ((*>))
import Control.MonadPlus (class MonadPlus)
import Control.Plus (class Plus)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.Monoid (class Monoid, mempty)
import Data.Set as Set
import Data.String as String
import Data.Tuple (Tuple(..))

import Data.Eulalie.Error as Error
import Data.Eulalie.Result as Result
import Data.Eulalie.Result (ParseResult())
import Data.Eulalie.Stream as Stream
import Data.Eulalie.Stream (Stream())

newtype Parser a = Parser (Stream -> ParseResult a)

parse :: forall a. Parser a -> Stream -> ParseResult a
parse (Parser parser) input = parser input

succeed :: forall a. a -> Parser a
succeed value = Parser \input -> Result.success value input input ""

fail :: forall a. Parser a
fail = Parser \input -> Result.error input

failAt :: forall a. Stream -> Parser a
failAt input = Parser \_ -> Result.error input

expected :: forall a. Parser a -> String -> Parser a
expected parser msg = Parser \input -> case parse parser input of
  Result.Error e -> Result.Error $ Error.withExpected e (Set.singleton msg)
  r -> r

item :: Parser Char
item = Parser \input -> case Stream.getAndNext input of
  Just { value, next } ->
    Result.success value next input (String.fromChar value)
  Nothing -> Result.error input

cut :: forall a. Parser a -> Parser a
cut parser = Parser \input -> case parse parser input of
  Result.Error r -> Result.Error $ Error.escalate r
  success -> success

seq :: forall a b. Parser a -> (a -> Parser b) -> Parser b
seq parser callback =
  Parser \input -> case parse parser input of
    Result.Success r -> case parse (callback r.value) r.next of
      Result.Success next -> Result.success next.value next.next input
                           (r.matched ++ next.matched)
      next -> next
    Result.Error err -> Result.Error err

either :: forall a. Parser a -> Parser a -> Parser a
either p1 p2 =
  Parser \input -> case parse p1 input of
    r@Result.Success _ -> r
    r@Result.Error { fatal: true } -> r
    Result.Error r1 -> case parse p2 input of
      r@Result.Success _ -> r
      Result.Error r2 -> Result.Error $ Error.extend r1 r2

withStart :: forall a. Parser a -> Parser (Tuple a Stream)
withStart parser = Parser \input -> case parse parser input of
  Result.Success r -> Result.Success $ r { value = Tuple r.value input }
  Result.Error err -> Result.Error err

sat :: (Char -> Boolean) -> Parser Char
sat predicate = do
  (Tuple v start) <- withStart item
  if predicate v then return v else failAt start

maybe :: Parser String -> Parser String
maybe parser = parser <|> return ""

eof :: Parser Unit
eof = expected eof' "end of file" where
  eof' :: Parser Unit
  eof' = Parser \input ->
    if Stream.atEnd input
      then Result.success unit input input ""
      else Result.error input

many :: forall a. Parser a -> Parser (List a)
many parser = many1 parser <|> return Nil

many1 :: forall a. Parser a -> Parser (List a)
many1 parser = do
  head <- parser
  tail <- many parser
  return (head : tail)

instance functorParser :: Functor Parser where
  map = liftM1

instance applyParser :: Apply Parser where
  apply = ap

instance applicativeParser :: Applicative Parser where
  pure = succeed

instance bindParser :: Bind Parser where
  bind = seq

instance monadParser :: Monad Parser

instance altParser :: Alt Parser where
  alt = either

instance plusParser :: Plus Parser where
  empty = fail

instance alternativeParser :: Alternative Parser

instance monadPlusParser :: MonadPlus Parser

instance semigroupParser :: (Monoid a) => Semigroup (Parser a) where
  append a b = append <$> a <*> b

instance monoidParser :: (Monoid a) => Monoid (Parser a) where
  mempty = return mempty
