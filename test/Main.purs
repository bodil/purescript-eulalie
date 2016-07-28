module Test.Main where

import Prelude
import Test.QuickCheck

import Data.String as String
import Data.String.Unsafe as UString
import Data.Tuple (Tuple(..))

import Data.Eulalie.Parser as P
import Data.Eulalie.Stream as Stream
import Data.Eulalie.Result as R

itemTest :: String -> Result
itemTest input =
  if String.null input then Success else
    let r = P.parse P.item $ Stream.stream input
        i0 = UString.charAt 0 input
        i1 = String.charAt 1 input
    in case r of
      R.Success s -> Tuple s.value (Tuple s.matched $ Stream.get s.next)
        === Tuple i0 (Tuple (String.singleton i0) i1)
      _ -> Failed $ "parse failed: \"" <> input <> "\""

main :: forall e. QC e Unit
main = do
  quickCheck itemTest
