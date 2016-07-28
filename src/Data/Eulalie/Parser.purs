module Data.Eulalie.Parser where

import Prelude
import Data.Eulalie.Error as Error
import Data.Eulalie.Result as Result
import Data.Eulalie.Stream as Stream
import Data.Set as Set
import Data.String as String
import Control.Alt (class Alt, (<|>))
import Control.Alternative (class Alternative)
import Control.Apply ((*>))
import Control.MonadPlus (class MonadPlus)
import Control.MonadZero (class MonadZero)
import Control.Plus (class Plus)
import Data.Eulalie.Result (ParseResult)
import Data.Eulalie.Stream (Stream)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.Monoid (class Monoid, mempty)
import Data.Tuple (Tuple(..))

newtype Parser a = Parser (Stream -> ParseResult a)

-- |Run a parse operation on a stream.
parse :: forall a. Parser a -> Stream -> ParseResult a
parse (Parser parser) input = parser input

-- |The `succeed` parser constructor creates a parser which will simply
-- |return the value provided as its argument, without consuming any input.
-- |
-- |This is equivalent to the monadic `pure`.
succeed :: forall a. a -> Parser a
succeed value = Parser \input -> Result.success value input input ""

-- |The `fail` parser will just fail immediately without consuming any input.
fail :: forall a. Parser a
fail = Parser \input -> Result.error input

-- |The `failAt` parser will fail immediately without consuming any input,
-- |but will report the failure at the provided input position.
failAt :: forall a. Stream -> Parser a
failAt input = Parser \_ -> Result.error input

-- |A parser combinator which returns the provided parser unchanged, except
-- |that if it fails, the provided error message will be returned in the
-- |`ParseError`.
expected :: forall a. Parser a -> String -> Parser a
expected parser msg = Parser \input -> case parse parser input of
  Result.Error e -> Result.Error $ Error.withExpected e (Set.singleton msg)
  r -> r

-- |The `item` parser consumes a single character, regardless of what it is,
-- |and returns it as its result.
item :: Parser Char
item = Parser \input -> case Stream.getAndNext input of
  Just { value, next } ->
    Result.success value next input (String.singleton value)
  Nothing -> Result.error input

-- |The `cut` parser combinator takes a parser and produces a new parser for
-- |which all errors are fatal, causing `either` to stop trying further
-- |parsers and return immediately with a fatal error.
cut :: forall a. Parser a -> Parser a
cut parser = Parser \input -> case parse parser input of
  Result.Error r -> Result.Error $ Error.escalate r
  success -> success

-- |Takes two parsers `p1` and `p2`, returning a parser which will match
-- |`p1` first, discard the result, then either match `p2` or produce a fatal
-- |error.
cutWith :: forall a b. Parser b -> Parser a -> Parser a
cutWith p1 p2 = p1 *> cut p2

-- |The `seq` combinator takes a parser, and a function which will receive
-- |the result of that parser if it succeeds, and which should return another
-- |parser, which will be run immediately after the initial parser. In this
-- |way, you can join parsers together in a sequence, producing more complex
-- |parsers.
-- |
-- |This is equivalent to the monadic `bind` operation.
seq :: forall a b. Parser a -> (a -> Parser b) -> Parser b
seq parser callback =
  Parser \input -> case parse parser input of
    Result.Success r -> case parse (callback r.value) r.next of
      Result.Success next -> Result.success next.value next.next input
                           (r.matched <> next.matched)
      next -> next
    Result.Error err -> Result.Error err

-- |The `either` combinator takes two parsers, runs the first on the input
-- |stream, and if that fails, it will backtrack and attempt the second
-- |parser on the same input. Basically, try parser 1, then try parser 2.
-- |
-- |If the first parser fails with an error flagged as fatal (see `cut`),
-- |the second parser will not be attempted.
-- |
-- |This is equivalent to the `alt` operation of `MonadPlus`/`Alt`.
either :: forall a. Parser a -> Parser a -> Parser a
either p1 p2 =
  Parser \input -> case parse p1 input of
    r@Result.Success _ -> r
    r@Result.Error { fatal: true } -> r
    Result.Error r1 -> case parse p2 input of
      r@Result.Success _ -> r
      Result.Error r2 -> Result.Error $ Error.extend r1 r2

-- |Converts a parser into one which will return the point in the stream where
-- |it started parsing in addition to its parsed value.
-- |
-- |Useful if you want to keep track of where in the input stream a parsed
-- |token came from.
withStart :: forall a. Parser a -> Parser (Tuple a Stream)
withStart parser = Parser \input -> case parse parser input of
  Result.Success r -> Result.Success $ r { value = Tuple r.value input }
  Result.Error err -> Result.Error err

-- |The `sat` parser constructor takes a predicate function, and will consume
-- |a single character if calling that predicate function with the character
-- |as its argument returns `true`. If it returns `false`, the parser will
-- |fail.
sat :: (Char -> Boolean) -> Parser Char
sat predicate = do
  (Tuple v start) <- withStart item
  if predicate v then pure v else failAt start

-- |The `maybe` parser combinator creates a parser which will run the provided
-- |parser on the input, and if it fails, it will returns the empty value (as
-- |defined by `mempty`) as a result, without consuming any input.
maybe :: forall a. (Monoid a) => Parser a -> Parser a
maybe parser = parser <|> pure mempty

-- |Matches the end of the stream.
eof :: Parser Unit
eof = expected eof' "end of file" where
  eof' :: Parser Unit
  eof' = Parser \input ->
    if Stream.atEnd input
      then Result.success unit input input ""
      else Result.error input

-- |The `many` combinator takes a parser, and returns a new parser which will
-- |run the parser repeatedly on the input stream until it fails, returning
-- |a list of the result values of each parse operation as its result, or the
-- |empty list if the parser never succeeded.
-- |
-- |Read that as "match this parser zero or more times and give me a list of
-- |the results."
many :: forall a. Parser a -> Parser (List a)
many parser = many1 parser <|> pure Nil

-- |The `many1` combinator is just like the `many` combinator, except it
-- |requires its wrapped parser to match at least once. The resulting list is
-- |thus guaranteed to contain at least one value.
many1 :: forall a. Parser a -> Parser (List a)
many1 parser = do
  head <- parser
  tail <- many parser
  pure (head : tail)

-- |Matches the provided parser `p` zero or more times, but requires the
-- |parser `sep` to match once in between each match of `p`. In other words,
-- |use `sep` to match separator characters in between matches of `p`.
sepBy :: forall a b. Parser a -> Parser b -> Parser (List b)
sepBy sep p = sepBy1 sep p <|> pure Nil

-- |Matches the provided parser `p` one or more times, but requires the
-- |parser `sep` to match once in between each match of `p`. In other words,
-- |use `sep` to match separator characters in between matches of `p`.
sepBy1 :: forall a b. Parser a -> Parser b -> Parser (List b)
sepBy1 sep p = do
  head <- p
  tail <- either (many $ sep *> p) $ pure Nil
  pure $ Cons head tail

-- |Like `sepBy`, but cut on the separator, so that matching a `sep` not
-- |followed by a `p` will cause a fatal error.
sepByCut :: forall a b. Parser a -> Parser b -> Parser (List b)
sepByCut sep p = do
  head <- p
  tail <- either (many (cutWith sep p)) $ pure Nil
  pure $ Cons head tail

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

instance monadZeroParser :: MonadZero Parser

instance monadPlusParser :: MonadPlus Parser

instance semigroupParser :: (Monoid a) => Semigroup (Parser a) where
  append a b = append <$> a <*> b

instance monoidParser :: (Monoid a) => Monoid (Parser a) where
  mempty = pure mempty
