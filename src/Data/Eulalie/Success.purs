module Data.Eulalie.Success where

import Prelude

import Data.Eulalie.Stream (Stream())

type ParseSuccess a = { value :: a, next :: Stream, start :: Stream,
                        matched :: String }
