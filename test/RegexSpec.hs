module RegexSpec (spec) where

import Data.ByteString (ByteString)
import Data.Text (Text)
import Test.Hspec (Spec, describe, it, shouldBe)
import Text.Regex.PCRE.Heavy (gsub, re, scan, sub, (=~))

spec :: Spec
spec = describe "Regex" $ do
    it "matches a-z" $ do
        let regex = [re|[a-z]|]
        ("123" :: Text) =~ regex `shouldBe` False
        ("12a" :: Text) =~ regex `shouldBe` True
        -- It also accepts any string-like argument
        ("12a" :: String) =~ regex `shouldBe` True
        ("12a" :: ByteString) =~ regex `shouldBe` True

    it "captures groups" $ do
        let regex = [re|^(hell.?), (.+)!$|]
        scan regex ("hello, world!" :: Text) `shouldBe` [("hello, world!", ["hello", "world"])]
        scan regex ("hell, world!" :: Text) `shouldBe` [("hell, world!", ["hell", "world"])]
        shouldBe (scan regex ("help, world!" :: Text)) []
        let regex2 = [re|(A+)|]
        scan regex2 ("AA" :: Text) `shouldBe` [("AA", ["AA"])]
        scan regex2 ("AAB" :: Text) `shouldBe` [("AA", ["AA"])]
        {-
        On the left : the matching segment
        On the right: the captured groups
         -}
        scan regex2 ("AABA.AAAxxx" :: Text)
            `shouldBe` [ ("AA", ["AA"])
                       , ("A", ["A"])
                       , ("AAA", ["AAA"])
                       ]

    it "replaces patterns with sub or gsub" $ do
        let regex = [re|[0-9]|]
        sub regex ("." :: Text) ("Hello123" :: Text) `shouldBe` "Hello.23"
        gsub regex ("" :: Text) ("Hello123" :: Text) `shouldBe` "Hello"