module Main (main) where

import qualified Domain.ValidationSpec
import Test.Hspec (hspec)

main :: IO ()
main = hspec $ do
    Domain.ValidationSpec.spec
