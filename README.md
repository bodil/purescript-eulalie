# purescript-eulalie

Fast string parser combinators for PureScript.

## Documentation

Read the [API documentation](https://pursuit.purescript.org/packages/purescript-eulalie/).

## Usage

Eulalie works on the principle of constructing parsers from smaller
parsers using various combinator functions.

A parser is a function which takes an input `Stream`, and returns a
`ParseResult` value which can be either a success or an error.

The type of parsers is defined like this:

```purescript
newtype Parser a = Parser (Stream -> ParseResult a)
```

### Data Types

```purescript
newtype Stream = Stream { buffer :: String, cursor :: Int }
```

A `Stream` just contains a string, and an index into this string. We
use this structure instead of passing strings around as input because
string operations are expensive, while any operation on the `Stream`
can be performed in linear time, and while many `Stream`s will be
created during a parse operation, we only ever keep a single copy of
the string they wrap.

```purescript
data ParseResult a = Success (ParseSuccess a)
                   | Error ParseError
```

A `ParseResult` is what's returned from a parser, and signals whether
it succeeded or failed. It wraps one of two result values,
`ParseSuccess` and `ParseError`.

```purescript
type ParseSuccess a = { value :: a,
                        next :: Stream,
                        start :: Stream,
                        matched :: String }
```

A `ParseSuccess` contains four properties: the `value` we parsed (an
arbitrary value), the `next` input to be parsed (a `Stream`), the
point in the stream where we `start`ed parsing (also a `Stream`), and
the substring that was `matched` by this parser (a string).

```purescript
type ParseError = { input :: Stream,
                    expected :: Set String,
                    fatal :: Boolean }
```

Finally, a `ParseError` simply contains an `input` property (a
`Stream`) which points to the exact position where the parsing failed,
and an optional `message` (a string). It also contains a `fatal` flag,
which signifies to the `either` combinator that we should stop parsing
immediately instead of trying further parsers.

### Parser Combinators

The most basic parsers form the building blocks from which you can
assemble more complex parsers:

  * `succeed :: forall a. a -> Parser a` makes a parser which doesn't
    consume input, just returns the provided value wrapped in a
    `ParseSuccess`.
  * `fail :: forall a. Parser a` is a parser which consumes no input
    and returns a `ParseError`.
  * `item :: Parser Char` is a parser which consumes one arbitrary
    character and returns it as a `ParseSuccess`.

The two fundamental parser combinators are:

  * `seq :: forall a b. Parser a -> (a -> Parser b) -> Parser b` is
    used to combine multiple parsers in a sequence. It takes a parser,
    and a function which will be called with the result of the parser
    if it succeeded, and must return another parser, which will be run
    on the remaining input. The result of the combined parser will be
    the result of this last parser, or the first error encountered.

    (This corresponds to the
    [`bind`/`>>=`](https://pursuit.purescript.org/packages/purescript-prelude/0.1.4/docs/Prelude#v:bind)
    method on the `Monad` type class.)

  * `either(parser1, parser2)` makes a parser which will first try the
    first provided parser, and returns its result if it succeeds. If
    it fails, it will run the second parser on the same input, and
    return its result directly, whether or not it succeeded.

    (This corresponds to the
    [`alt`/`<|>`](https://pursuit.purescript.org/packages/purescript-control/0.3.2/docs/Control.Alt#v:alt)
    method on the `Alt` type class.)

Using these, you can construct more advanced parser combinators. Some particularly useful combinators are predefined:

  * `sat :: (Char -> Boolean) -> Parser Char` makes a parser which
    will match one character only if the provided predicate function
    returns `true` for it.
  * `char :: Char -> Parser Char` makes a parser which matches a
    specific single character.
  * `many :: forall a. Parser a -> Parser (List a)` makes a parser
    which will match the provided parser zero or more times.
  * `many1 :: forall a. Parser a -> Parser (List a)` works just like
    `many`, but requires at minimum one match.
  * `string :: String -> Parser String` makes a parser which matches
    the provided string exactly (which is done by using the `char`
    parser for each `Char` in the string).

Other predefined parsers are `digit`, `space`, `alphanum`, `letter`,
`upper` and `lower`, which match one character of their respective
types, and their inverse counterparts, `notDigit`, `notSpace`,
`notAlphanum`, `notLetter`, `notUpper` and `notLower`. There are also
whitespace matchers `spaces` and `spaces1`, and their opposites,
`notSpaces` and `notSpaces1`.

### Do Notation

Because parsers implement the `Monad` type class, and the `seq`
combinator is actually the monadic bind operation, you can combine
parsers using do notation, like this:

```purescript
import Data.Eulalie.Parser as P
import Data.Eulalie.String as S

-- a parser for the string "hi <your name>!",
-- returning the "<your name>" part.

-- without do notation:
myParser = P.seq (S.string "hi") \_ ->
  P.seq S.spaces1 \_ ->
    P.seq S.notSpaces1 \name ->
      P.seq S.string "!" \_ ->
        P.succeed name

-- with do notation:
myParser = do
  S.string "hi"
  S.spaces1
  name <- S.notSpaces1
  S.string "!"
  return name
```

### Alternatives

Because `Parser` implements the `MonadPlus` type class (and `either`
is the implementation for `alt`), you can also use this alternative
syntax for trying several parsers until one succeeds:

```purescript
import Control.Alt ((<|>))
import Data.Eulalie.Parser as P
import Data.Eulalie.String as S

-- a parser matching one of the strings "lol", "rofl" and "lmao"

-- using `either`:
myParser = P.either (S.string "lol")
  (P.either (S.string "rofl" S.String "lmao"))

-- using <|>:
myParser = S.string "lol" <|> S.string "rofl" <|> S.string "lmao"
```

### Monoids

For a `Parser a` where `a` is a monoid, there's a type class
implementation for `(Monoid a) => Monoid (Parser a)`, so that you can
treat parsers for monoids like they're monoids too.

What this means, practically, is that because strings are monoids, you
can do things like this for parsers of type `Parser String`,
concatenating the results of each parser into a final result:

```purescript
import Data.Eulalie.Parser as P
import Data.Eulalie.String as S
import Data.Foldable (fold)

-- a parser matching the whole string "hi <your name>!"

-- using the semigroup append operator:
myParser = S.string "hi" <> S.spaces1 <> S.notSpaces1 <> S.string "!"

-- using a fold over a list of parsers:
myParser = fold [S.string "hi", S.spaces1, S.notSpaces1, S.string "!"]
```

### A Working Example

This is how you might write a parser for the first line of an HTTP
request:

```purescript
import Data.Eulalie.Parser as P
import Data.Eulalie.String as S
import Data.Eulalie.Char as C
import Data.Eulalie.Stream (stream)

type HTTPRequest = { method :: String,
                     path :: String,
                     version :: String }

parser :: Parser HTTPRequest
parser = do
  -- Parse a sequence of 1 or more upper case letters
  method <- C.many1 C.upper
  -- Consume 1 or more spaces
  S.spaces1
  -- Parse a sequence of 1 or more non-whitespace characters
  path <- S.notSpaces1
  -- Consume 1 or more spaces
  S.spaces1
  -- Match the string "HTTP/"
  S.string "HTTP/"
  -- Parse the version string
  version <- C.many1 C.digit <> S.string "." <> C.many1 C.digit
  -- Return the final parsed value
  return { method, path, version }

result = P.parse parser (stream "GET /lol.gif HTTP/1.0")
-- { method:  "GET",
--   path:    "/lol.gif",
--   version: "1.0" }
```

## Licence

Copyright 2015 Bodil Stokke

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU Lesser General Public License as
published by the Free Software Foundation, either version 3 of the
License, or (at your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this program. If not, see
<http://www.gnu.org/licenses/>.
