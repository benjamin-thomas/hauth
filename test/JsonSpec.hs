{-# LANGUAGE LambdaCase #-}

module JsonSpec (spec) where

import Data.Aeson (
    FromJSON (parseJSON),
    KeyValue ((.=)),
    Options (fieldLabelModifier),
    Result (Error, Success),
    ToJSON (toJSON),
    Value (Array, Bool, Null, Number, Object, String),
    decode,
    defaultOptions,
    eitherDecode,
    encode,
    fromJSON,
    genericParseJSON,
    genericToJSON,
    object,
    (.:),
 )

import qualified Data.Aeson.KeyMap as KeyMap
import Data.Aeson.TH (deriveJSON)
import Data.Aeson.Types (Parser)
import Data.ByteString.Lazy (ByteString)
import Data.Char (toLower)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Vector as V
import GHC.Generics (Generic)
import Language.Haskell.TH (nameBase)
import Test.Hspec (
    Example (Arg),
    Spec,
    SpecWith,
    describe,
    it,
    shouldBe,
 )
import Text.RawString.QQ (r)
import Text.Regex.PCRE.Heavy (re, sub)

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

test :: (Example a) => String -> a -> SpecWith (Arg a)
test = it

data Animal = Animal
    { animalName :: Text
    , animalAge :: Int
    , animalIsCute :: Bool
    }
    deriving (Eq, Show)

dropPrefix :: String -> String -> String
dropPrefix prefix = lowerFirst . drop (length prefix)
  where
    lowerFirst :: String -> String
    lowerFirst = \case
        [] -> []
        (x : xs) -> toLower x : xs

-- Must appear before usage!
$( let
    name = nameBase ''Animal

    -- TH restriction: can't define locally so I'll just duplicate for now
    dropPrefix' :: String -> String -> String
    dropPrefix' prefix = lowerFirst . drop (length prefix)
      where
        lowerFirst :: String -> String
        lowerFirst = \case
            [] -> []
            (x : xs) -> toLower x : xs

    options =
        defaultOptions
            { fieldLabelModifier = dropPrefix' name
            }
    in
    deriveJSON options ''Animal
 )

-- We can automatically create codecs with DeriveGeneric or TemplateHaskell
data ProgrammingLanguage = ProgrammingLanguage
    { plName :: Text
    , plReleasedOn :: Int
    , plIsFunctional :: Bool
    }
    deriving (Eq, Show, Generic)

data StockItem
    = BallPointPen
    | Book Text
    | Toy Double Bool
    | Car
        { carMaker :: Text
        , carNumberOfSeats :: Int
        , carIsElectric :: Bool
        }
    | Computer
        { compMaker :: Text
        , compNumberOfCores :: Int
        }
    deriving (Eq, Show, Generic)

$( let
    lowerFirst :: String -> String
    lowerFirst [] = []
    lowerFirst (x : xs) = toLower x : xs

    toStrip = [re|^car|^comp|]
    in
    deriveJSON
        defaultOptions
            { fieldLabelModifier = lowerFirst . sub toStrip ("" :: Text)
            }
        ''StockItem
 )

instance FromJSON ProgrammingLanguage where
    parseJSON =
        genericParseJSON
            defaultOptions
                { fieldLabelModifier = dropPrefix "pl"
                }

instance ToJSON ProgrammingLanguage where
    toJSON =
        genericToJSON
            defaultOptions
                { fieldLabelModifier = dropPrefix "pl"
                }

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

    it "knows how to encode known Haskell types" $ do
        -- Maybe and Either implement toJSON
        encode (Just "ABC" :: Maybe Text) `shouldBe` [r|"ABC"|]
        encode (Right 123 :: Either Text Int) `shouldBe` [r|{"Right":123}|]
        encode (Left "Oops" :: Either Text Int) `shouldBe` [r|{"Left":"Oops"}|]

    test "eitherDecode" $ do
        shouldBe
            (Right 123)
            (eitherDecode [r|123|] :: Either String Int)

        shouldBe
            (Right $ Number 123.0)
            (eitherDecode [r|123|] :: Either String Value)

        shouldBe
            ( Right
                ( Object
                    ( KeyMap.fromList [("Right", Number 123.0)]
                    )
                )
            )
            (eitherDecode [r|{"Right":123}|] :: Either String Value)

        shouldBe
            ( Left
                [r|Unexpected "wat", expecting JSON value|]
            )
            (eitherDecode [r|wat|] :: Either String Value)

        shouldBe
            ( Right
                ( Object
                    ( KeyMap.fromList
                        [ ("userId", Number 123)
                        , ("name", String "John")
                        , ("isAdmin", Bool True)
                        , ("xy", toJSON [Number 1.0, Number 2.0, String "wat"])
                        , ("yz", Array (V.fromList [Number 3.0, Number 4.0, String "wow"]))
                        ]
                    )
                )
            )
            (eitherDecode [r|{"userId":123,"name":"John","isAdmin":true,"xy":[1,2,"wat"],"yz":[3,4,"wow"]}|] :: Either String Value)

        -- We can decode some clean types
        shouldBe
            (Right $ Map.fromList [(1, "John"), (2, "Jane"), (3, "Bob")])
            (eitherDecode [r|{"1":"John","2":"Jane","3":"Bob"}|] :: Either String (Map Int Text))

        -- We can decode some crazy types
        shouldBe
            ( Right
                ( Map.fromList
                    [
                        ( 1
                        , String "John"
                        )
                    ,
                        ( 2
                        , Object
                            ( KeyMap.fromList
                                [ ("female", Bool True)
                                , ("name", String "Jane")
                                ]
                            )
                        )
                    ]
                )
            )
            (eitherDecode [r|{"1":"John","2":{"female":true,"name":"Jane"}}|] :: Either String (Map Int Value))

    test "we can automatically create codecs with DeriveGeneric" $ do
        let input :: ByteString
            input = [r|{"name":"Haskell","releasedOn":1990,"isFunctional":true}|]

            haskell :: ProgrammingLanguage
            haskell =
                ProgrammingLanguage
                    { plName = "Haskell"
                    , plReleasedOn = 1990
                    , plIsFunctional = True
                    }
        shouldBe
            (Right haskell)
            (eitherDecode input :: Either String ProgrammingLanguage)

        shouldBe
            -- keys in alphabetical order
            ([r|{"isFunctional":true,"name":"Haskell","releasedOn":1990}|] :: ByteString)
            (encode haskell)

    test "we can automatically create codecs with TemplateHaskell" $ do
        let input :: ByteString
            input = [r|{"name":"Horse","age":10,"isCute":true}|]

            horse :: Animal
            horse =
                Animal
                    { animalName = "Horse"
                    , animalAge = 10
                    , animalIsCute = True
                    }
        shouldBe
            (Right horse)
            (eitherDecode input :: Either String Animal)

        shouldBe
            -- keys in declaration order
            ([r|{"name":"Horse","age":10,"isCute":true}|] :: ByteString)
            (encode horse)

    it "encodes custom types" $ do
        shouldBe
            (encode BallPointPen)
            [r|{"tag":"BallPointPen"}|]

        shouldBe
            (encode $ Book "Practical Common Lisp")
            [r|{"tag":"Book","contents":"Practical Common Lisp"}|]

        shouldBe
            (encode $ Toy 123.45 True)
            [r|{"tag":"Toy","contents":[123.45,true]}|]

        shouldBe
            ( encode $
                Car
                    { carMaker = "Ford"
                    , carIsElectric = True
                    , carNumberOfSeats = 5
                    }
            )
            [r|{"tag":"Car","maker":"Ford","numberOfSeats":5,"isElectric":true}|]

        shouldBe
            (encode $ Computer{compMaker = "Apple", compNumberOfCores = 8})
            [r|{"tag":"Computer","maker":"Apple","numberOfCores":8}|]

    it "decodes custom types" $ do
        shouldBe
            (eitherDecode [r|{"tag":"BallPointPen"}|] :: Either String StockItem)
            (Right BallPointPen)

        shouldBe
            (eitherDecode [r|{"tag":"Book","contents":"Practical Common Lisp"}|] :: Either String StockItem)
            (Right $ Book "Practical Common Lisp")

        shouldBe
            (eitherDecode [r|{"tag":"Toy","contents":[123.45,true]}|] :: Either String StockItem)
            (Right $ Toy 123.45 True)

        shouldBe
            (eitherDecode [r|{"tag":"Car","maker":"Ford","numberOfSeats":5,"isElectric":true}|] :: Either String StockItem)
            ( Right $
                Car
                    { carMaker = "Ford"
                    , carIsElectric = True
                    , carNumberOfSeats = 5
                    }
            )

        shouldBe
            (eitherDecode [r|{"tag":"Computer","maker":"Apple","numberOfCores":8}|] :: Either String StockItem)
            ( Right $
                Computer
                    { compMaker = "Apple"
                    , compNumberOfCores = 8
                    }
            )