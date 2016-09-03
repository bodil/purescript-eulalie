module Data.Eulalie.Success where

import Data.Eulalie.Stream (Stream)

type ParseSuccess i o =
  { value :: o
  , next :: Stream i
  , start :: Stream i
  , matched :: Array i
  }
