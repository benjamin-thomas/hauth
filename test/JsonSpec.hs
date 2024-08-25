{-# LANGUAGE LambdaCase #-}

module JsonSpec (spec) where

import Data.Aeson (
    FromJSON (parseJSON),
    KeyValue ((.=)),
    Result (Error, Success),
    ToJSON (toJSON),
    Value (Null, Object),
    decode,
    encode,
    fromJSON,
    object,
    (.:),
 )
import Data.Aeson.Types (Parser)
import Data.ByteString.Lazy (ByteString)
import Data.Text (Text)
import Test.Hspec (Spec, describe, it, shouldBe)
import Text.RawString.QQ (r)

data User = User
    { userId :: Int
    , userName :: Text
    , userCountry :: Maybe Text
    , userHobbies :: [Text]
    }
    deriving (Eq, Show)

instance FromJSON User where
    parseJSON = userParser'

-- | This parser IS sensitive to the order of the fields
_userParser :: Value -> Parser User
_userParser = \case
    Object v ->
        User
            <$> v .: "id"
            <*> v .: "name"
            <*> v .: "hobbies"
            <*> v .: "country"
    _ -> fail "User: expected an object"

-- | This parser IS NOT sensitive to the order of the fields
userParser' :: Value -> Parser User
userParser' = \case
    Object v -> do
        userId' <- v .: "id"
        userName' <- v .: "name"
        userHobbies' <- v .: "hobbies"
        userCountry' <- v .: "country"
        pure
            User
                { userId = userId'
                , userName = userName'
                , userHobbies = userHobbies'
                , userCountry = userCountry'
                }
    _ -> fail "User: expected an object"

spec :: Spec
spec = describe "Json" $ do
    it "encodes JSON" $ do
        let output :: Value
            output =
                object
                    [ "id" .= (123 :: Int)
                    , "name" .= ("John" :: Text)
                    , "hobbies" .= (["Music", "Art"] :: [Text])
                    , "country" .= Null
                    ]
        let input :: ByteString
            input =
                [r|
            {
                "id": 123,
                "name": "John",
                "hobbies": [
                    "Music",
                    "Art"
                ],
                "country": null
            }
            |]
        decode input `shouldBe` Just output
        decode (encode output) `shouldBe` Just output

    it "maps JSON values to Haskell types" $ do
        let value :: Value
            value =
                object
                    [ "id" .= (123 :: Int)
                    , "name" .= ("John" :: Text)
                    , "hobbies" .= (["Music", "Art"] :: [Text])
                    , "country" .= Null
                    ]
        let userResult :: Result User -- weird that the type isn't Either!
            userResult = fromJSON value

        userResult
            `shouldBe` ( Success $
                            User
                                { userId = 123
                                , userName = "John"
                                , userHobbies = ["Music", "Art"]
                                , userCountry = Nothing
                                }
                       )

        let bogusValue :: Value
            bogusValue = toJSON ("bogus" :: Text)

        let bogusResult :: Result User
            bogusResult = fromJSON bogusValue

        bogusResult `shouldBe` Error "User: expected an object"
