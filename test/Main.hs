module Main (main) where

import qualified Domain.ValidationSpec
import qualified RegexSpec
import Test.Hspec (hspec)
import qualified TimeSpec

main :: IO ()
main = hspec $ do
    Domain.ValidationSpec.spec
    TimeSpec.spec
    RegexSpec.spec
