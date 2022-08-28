module Domain.ValidationSpec (spec) where

import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec =
    describe "Validation" $ do
        it "works" $ do
            1 + 1 `shouldBe` (2 :: Int)