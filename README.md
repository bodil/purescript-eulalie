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
newtype Parser i o = Parser (Stream i -> ParseResult i o)
```

### Data Types

```purescript
newtype Stream i = Stream i { buffer :: Array i, cursor :: Int }
```

A `Stream` just contains an array of input data, and an index into
this array. We use this structure instead of passing arrays around as
input because array operations are expensive, while any operation on
the `Stream` can be performed in linear time, and while many `Stream`s
will be created during a parse operation, we only ever keep a single
copy of the array they wrap.

```purescript
data ParseResult i o = Success (ParseSuccess i o)
                     | Error (ParseError i)
```

A `ParseResult` is what's returned from a parser, and signals whether
it succeeded or failed. It wraps one of two result values,
`ParseSuccess` and `ParseError`.

```purescript
type ParseSuccess i o =
  { value :: o
  , next :: Stream i
  , start :: Stream i
  , matched :: Array i
  }
```

A `ParseSuccess` contains four properties: the `value` we parsed (an
arbitrary value), the `next` input to be parsed (a `Stream`), the
point in the stream where we `start`ed parsing (also a `Stream`), and
the sub-array that was `matched` by this parser.

```purescript
type ParseError i =
  { input :: Stream i
  , expected :: Set String
  , fatal :: Boolean
  }
```

Finally, a `ParseError` simply contains an `input` property (a
`Stream`) which points to the exact position where the parsing failed,
and a set of string descriptions of expected inputs. It also contains
a `fatal` flag, which signifies to the `either` combinator that we
should stop parsing immediately instead of trying further parsers.

### Parser Combinators

The most basic parsers form the building blocks from which you can
assemble more complex parsers:

  * `succeed :: forall i o. o -> Parser i o` makes a parser which
    doesn't consume input, just returns the provided value wrapped in
    a `ParseSuccess`.
  * `fail :: forall i o. Parser i o` is a parser which consumes no
    input and returns a `ParseError`.
  * `item :: forall i. Parser i i` is a parser which consumes one
    arbitrary input value and returns it as a `ParseSuccess`.

The two fundamental parser combinators are:

  * `seq :: forall i a b. Parser i a -> (a -> Parser i b) -> Parser i
    b` is used to combine multiple parsers in a sequence. It takes a
    parser, and a function which will be called with the result of the
    parser if it succeeded, and must return another parser, which will
    be run on the remaining input. The result of the combined parser
    will be the result of this last parser, or the first error
    encountered.

    (This corresponds to the
    [`bind`/`>>=`](https://pursuit.purescript.org/packages/purescript-prelude/0.1.4/docs/Prelude#v:bind)
    method on the `Monad` type class.)

  * `either :: forall i o. Parser i o -> Parser i o -> Parser i o`
    makes a parser which will first try the first provided parser, and
    returns its result if it succeeds. If it fails, it will run the
    second parser on the same input, and return its result directly,
    whether or not it succeeded.

    If you've heard the term "backtracking" in relation to parsers,
    this is handled automatically by the `either` function, and you
    don't need to worry about it.

    (This corresponds to the
    [`alt`/`<|>`](https://pursuit.purescript.org/packages/purescript-control/0.3.2/docs/Control.Alt#v:alt)
    method on the `Alt` type class.)

Using these, you can construct more advanced parser combinators. Some particularly useful combinators are predefined:

  * `sat :: forall i. (i -> Boolean) -> Parser i i` makes a parser
    which will match one input value only if the provided predicate
    function returns `true` for it.
  * `many :: forall i o. Parser i o -> Parser i (List o)` makes a
    parser which will match the provided parser zero or more times.
  * `many1 :: forall i o. Parser i o -> Parser i (List o)` works just
    like `many`, but requires at minimum one match.
  * `char :: Char -> Parser Char Char` makes a parser which matches a
    specific single character.
  * `string :: String -> Parser Char String` makes a parser which
    matches the provided string exactly (which is done by using the
    `char` parser for each `Char` in the string).

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
      P.seq (S.string "!") \_ ->
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
  (P.either (S.string "rofl") (S.string "lmao"))

-- using <|>:
myParser = S.string "lol" <|> S.string "rofl" <|> S.string "lmao"
```

### Monoids

For a `Parser i o` where `o` is a monoid, there's a type class
implementation for `(Monoid o) => Monoid (Parser i o)`, so that you
can treat parsers for monoids like they're monoids too.

What this means, practically, is that because strings are monoids, you
can do things like this for parsers of type `Parser Char String`,
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
import Data.String (toCharArray)

type HTTPRequest = { method :: String,
                     path :: String,
                     version :: String }

parser :: Parser Char HTTPRequest
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

result = P.parse parser (stream $ toCharArray "GET /lol.gif HTTP/1.0")
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
