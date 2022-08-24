{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module MyLib (hello) where

import Data.Aeson.TH (deriveJSON)
import Extra (someFn)

import Data.Aeson (
    FromJSON,
    KeyValue ((.=)),
    Options (constructorTagModifier, fieldLabelModifier),
    ToJSON (toEncoding),
    Value (Null),
    defaultOptions,
    eitherDecode,
    encode,
    fromJSON,
    genericToEncoding,
    object,
    pairs,
    parseJSON,
    toJSON,
    withObject,
    (.:),
 )

import qualified Data.Aeson as Aeson (Result)
import Data.Time (
    NominalDiffTime,
    UTCTime,
    ZonedTime,
    defaultTimeLocale,
    diffUTCTime,
    getCurrentTime,
    getZonedTime,
    iso8601DateFormat,
    minutesToTimeZone,
    parseTimeM,
    utcToLocalZonedTime,
    utcToZonedTime,
    zonedTimeToUTC,
 )

import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import GHC.Generics (Generic)
import Text.Regex.PCRE.Heavy (Regex, gsub, re, scan, (=~))

import NriPrelude (Bool (True), Char, Eq, Float, Int, List, Maybe (Just, Nothing), Result, Show, Text, (*), (++), (-), (/), (<), (<|), (==), (>), (|>))

import Prelude (Double, IO, Integer, String, drop, subtract, ($), (.), (<$>), (<*>), (<>))
import qualified Prelude (Int, Integer, String, map)

import Data.Either (Either (Left, Right))

import Result (Result (Err, Ok), withDefault)
import Text (fromInt, isEmpty, repeat, reverse, toInt, toList, toLower, toUpper, trim)

import Data.Fixed (Pico)
import Data.Time.Lens (ZonedTime, day, getL, getZonedTime, hours, minutes, modL, month, seconds, setL, year)

{-
Pass strings to `Main.hs`:
    Prelude.putStrLn hello
 -}
{-# DEPRECATED hello "Use hello2" #-}
hello :: List Char
hello = "Hello world from lib!" |> toList

hello2 :: List Char
hello2 = "Hello world2 from lib!" |> toList

floatDiv :: Float
floatDiv = 3.14 / 2

emptyEx1 :: Bool
emptyEx1 = isEmpty ""

emptyEx2 :: Bool
emptyEx2 = " " |> trim |> isEmpty

aPalindrome :: [Text]
aPalindrome = ["noon", reverse "noon"]

-- https://hackage.haskell.org/package/nri-prelude-0.6.0.3/docs/Text.html
repeatEx :: Text
repeatEx = repeat 3 "!"

-- CONVERSION

intStr :: Text
intStr = fromInt 1_234

strInt :: Maybe Int
strInt = toInt "1234"

workflowEx :: Maybe Int -> Text
workflowEx maybeInt = case maybeInt of
    Just n -> "YES[" ++ fromInt n ++ "]"
    Nothing -> "NO!"

debugToStringTest :: [Text]
debugToStringTest =
    [ Debug.toString 42
    , Debug.toString [1, 2]
    , Debug.toString ('a', "b", 3)
    , Debug.toString True
    ]

debugLogTest :: Int
debugLogTest =
    Debug.log
        "Do something later..."
        3

data Animal = Cat | Dog | Fish

printSound :: Animal -> Text
printSound animal =
    case animal of
        Cat -> "Meow"
        Dog -> "Woof"
        _ ->
            Debug.todo "handle Fish later..."

testResult :: [Int]
testResult =
    [ Result.withDefault 0 (Ok 123)
    , Result.withDefault 0 (Err "no")
    ]

timeParserUTC :: [Char] -> Maybe UTCTime
timeParserUTC =
    parseTimeM
        True
        defaultTimeLocale
        (iso8601DateFormat (Just "%H:%M:%S%Q%z"))

timeParserZoned :: [Char] -> Maybe ZonedTime
timeParserZoned =
    parseTimeM
        True
        defaultTimeLocale
        (iso8601DateFormat (Just "%H:%M:%S%Q%z"))

testParseTimeUTC :: Maybe UTCTime
testParseTimeUTC =
    timeParserUTC "2022-08-21T14:00:00+0200"

testParseTimeZoned :: Maybe ZonedTime
testParseTimeZoned =
    timeParserZoned "2022-08-21T14:00:00+0200"

timeWasUTC :: UTCTime
timeWasUTC = posixSecondsToUTCTime 1661094986

timeIsUTC :: IO UTCTime
timeIsUTC = getCurrentTime

timeIsZoned :: IO ZonedTime
timeIsZoned = getZonedTime

tzTransforms :: (ZonedTime, UTCTime)
tzTransforms =
    let tz = minutesToTimeZone (60 * 2)
        timeWasTz = utcToZonedTime tz timeWasUTC
     in ( timeWasTz
        , timeWasTz |> zonedTimeToUTC
        )

-- Time can only be compared in UTC
timeComparison :: [Bool]
timeComparison =
    let tz = minutesToTimeZone (60 * 2)
     in [ posixSecondsToUTCTime 0 < posixSecondsToUTCTime 1
        , posixSecondsToUTCTime 1 > posixSecondsToUTCTime 0
        , posixSecondsToUTCTime 0 == posixSecondsToUTCTime 0
        , posixSecondsToUTCTime 0 < timeWasUTC
        , posixSecondsToUTCTime 0 < (timeWasUTC |> utcToZonedTime tz |> zonedTimeToUTC)
        ]

timeDistance :: [Text]
timeDistance =
    [ Debug.toString (diffUTCTime (posixSecondsToUTCTime 1) (posixSecondsToUTCTime 0))
    , Debug.toString (diffUTCTime (posixSecondsToUTCTime 0) (posixSecondsToUTCTime 1))
    ] -- ["1 s","-1s"]

diffTypeIs :: NominalDiffTime
diffTypeIs = diffUTCTime (posixSecondsToUTCTime 1) (posixSecondsToUTCTime 0)

mathOnDiff :: NominalDiffTime
mathOnDiff = diffTypeIs * 2 -- 2s

{-
Integer :: *
Defined in ‘GHC.Integer.Type’ (integer-gmp-1.0.3.0)

Arbitrary precision integers. In contrast with fixed-size integral types such as Int , the Integer type represents the entire infinite range of integers.

For more information about this type's representation, see the comments in its implementation.

---

Int :: *
Defined in ‘GHC.Types’ (ghc-prim-0.6.1)

A fixed-precision integer type with at least the range [-2^29 .. 2^29-1] .
The exact range for a given implementation can be determined by using Prelude.minBound and
Prelude.maxBound from the Prelude.Bounded class.

---

Pico

resolution of 10^-12 = .000000000001
-}
extractTimeComponents :: (Prelude.Integer, Prelude.Int, Prelude.Int, Prelude.Int, Prelude.Int, Pico)
extractTimeComponents =
    let t = timeWasUTC
     in ( getL year t
        , getL month t
        , getL day t
        , getL hours t
        , getL minutes t
        , getL seconds t
        )

extractTimeComponentsPrint :: Text
extractTimeComponentsPrint =
    let t = timeWasUTC
     in (Debug.toString t |> Debug.log)
            extractTimeComponents
            |> Debug.toString

alterTimeComponent :: (UTCTime, UTCTime, UTCTime, UTCTime)
alterTimeComponent =
    let t1 = timeWasUTC
        t2 = setL year 2000 . modL day (subtract 1) $ t1
        t3 = setL year 1900 . setL month 2 . setL day 29 $ t1 -- returns: 1900-03-01 15:16:26 UTC
        t4 = setL year 1900 . setL month 2 . setL day 30 $ t1 -- returns: 1900-03-02 15:16:26 UTC
     in (t1, t2, t3, t4)

regex1 :: Regex
regex1 = [re|^(hell.), (.+)!$|]

-- I'm not sure why I have to provide extra type information here...
-- Not necessary inside the REPL!
--
-- Normal usage:
--   "hello, world!" =~ regex1 => True
--
-- Reverse the call order:
--   (\x -> x =~ regex1) "hello, world!"
--      => True
--
--   compare = \x -> (x :: Text) =~ regex1
--
--   compare "hello, world!"
--     => True
reCompare :: Bool
reCompare = ("hello" :: Text) =~ regex1

reCompare2 :: Bool
reCompare2 = ("hello" :: [Char]) =~ regex1

{-
scan regex1 ("hello, world!" :: Text)
  => [("hello, world!",["hello","world"])]
-}
extractFromRe :: [(Text, [Text])]
extractFromRe = scan regex1 "hello, world!"

{-
The REPL wants the type hint in reverse order it seems...
gsub [re|\d+|] "X" ("Remove 123 OK" :: Text)
    => "Remove X OK"
-}
substituteFromRe :: Text
substituteFromRe = gsub [re|\d+|] ("X" :: Text) "Remove 123 OK"

{-
The REPL wants the type hint in reverse order it seems...
gsub [re|\d|] "x" ("Remove 123 OK" :: Text)
    => "Remove xxx OK"
-}
substituteFromReNonGreedy :: Text
substituteFromReNonGreedy = gsub [re|\d|] ("x" :: Text) "Remove 123 OK"

{-

{
    "id": 123,
    "name": "Benjamin",
    "hobbies": ["Programming", "Piano"],
    "country": null
}

Object (fromList
    [ ("country",Null)
    , ("hobbies"
    , Array [String "Programming",String "Piano"])
    , ("id",Number 123.0)
    , ("name",String "Benjamin")
    ])

-}
jsonEncodeExample :: Value
jsonEncodeExample =
    object
        [ "id" .= (123 :: Int)
        , "name" .= ("Benjamin" :: Text)
        , "hobbies" .= (["Programming", "Piano"] :: [Text])
        , "country" .= Null
        ]

jsonDecodeExample :: Aeson.Result Int
jsonDecodeExample =
    let j = toJSON (1 :: Int)
     in fromJSON j :: Aeson.Result Int -- => Success 1

enc1 = encode (Just "hello" :: Maybe Text)
enc2 = encode (Nothing :: Maybe Text)

-- enc3 = encode (1, "two", 3.3) -- don't know the type name!

-- START: Person example definition from the docs...
data Person = Person
    { name :: Text
    , age :: Int
    }
    deriving (Generic, Show)

instance ToJSON Person where
    -- No need to provide a toJSON implementation.

    -- For efficiency, we write a simple toEncoding implementation, as
    -- the default version uses toJSON.
    toEncoding = genericToEncoding defaultOptions

instance FromJSON Person

-- No need to provide a parseJSON implementation.
-- END: Person example definition from the docs...

enc4 = encode $ Person "Benjamin" 41 -- => "{\"name\":\"Benjamin\",\"age\":41}"

dec :: Either String Person
dec = eitherDecode enc4 -- => Right (Person {name = "Benjamin", age = 41})

dec2 :: Either String Person
dec2 = eitherDecode "{\"name\":\"Benjamin\",\"age2\":41}" -- => Left "Error in $: parsing MyLib.Person(Person) failed, key \"age\" not found"

-- Provide an explicit encode/decode implementation
data User = User
    { ident :: Int
    , firstName :: Text
    , occupations :: [Text]
    }
    deriving (Show)

instance ToJSON User where
    toJSON (User uId fn occs) =
        object
            [ "id" .= uId
            , "name" .= fn
            , "hobbies" .= occs
            ]

{- ORMOLU_DISABLE -}
instance FromJSON User where
    parseJSON = withObject "User" $ \v ->
        User <$> v .: "id"
             <*> v .: "name"
             <*> v .: "hobbies"
{- ORMOLU_ENABLE -}

enc5 = encode $ User 1 "Benjamin" ["Programming", "Piano"] -- => "{\"hobbies\":[\"Programming\",\"Piano\"],\"name\":\"Benjamin\",\"id\":1}

dec5 :: Either String User
dec5 =
    let input =
            "{\"hobbies\":[\"Programming\",\"Piano\"],\"name\":\"Benjamin\",\"id\":1}"
     in eitherDecode input -- Right (User {ident = 1, firstName = "Benjamin", occupations = ["Programming","Piano"]})

{-
NOTE:
    Code below will not compile and return the fellowing error:
        'Multiple declarations of ‘abc’'

data Apples  = Apples  {abc :: Int}
data Oranges = Oranges {abc :: Int}
-}

-- Prefix all fields with the type name to prevent name clashes

{-

This example from the book won't compile with `TemplateHaskell` extension enabled
I suppose the library has changed since and uses `DeriveGeneric` instead
deriveJSON is not exported by Data.Aeson
data Pet = Pet
    { petID :: Int
    , petOwnerName :: Text
    , petName :: Text
    }
    deriving (Show)

instance ToJSON Pet
    $(deriveJSON defaultOptions "Pet")

instance FromJSON Pet
    $(deriveJSON defaultOptions "Pet")
-}

-- Minimal setup below, requires Generic
data Pet = Pet
    { petID :: Int
    , petOwnerName :: Text
    , petName :: Text
    }
    deriving (Show, Generic)

instance ToJSON Pet

instance FromJSON Pet

enc6 = encode $ Pet 1 "John" "Kat" -- => "{\"petName\":\"Kat\",\"petID\":1,\"petOwnerName\":\"John\"}"

data Coord = Coord {x :: Double, y :: Double}

instance ToJSON Coord where
    toJSON (Coord x y) = object ["x" .= x, "y" .= y]

    toEncoding (Coord x y) = pairs ("x" .= x <> "y" .= y)

data Coord2 = Coord2
    { x2 :: Double
    , y2 :: Double
    }
    deriving (Show, Generic)
$(deriveJSON defaultOptions{fieldLabelModifier = someFn} ''Coord2)

{- I've got troubles with some types so this will do for now
MyLib> toJSON $ Coord2 1 2
Object (fromList [("x2x2",Number 1.0),("y2y2",Number 2.0)])
-}

data D a
    = Nullary
    | Unary Int
    | Product String Char a
    | Record
        { testOne :: Double
        , testTwo :: Bool
        , testThree :: D a
        }
    deriving (Eq, Show)

-- $(deriveJSON defaultOptions{fieldLabelModifier = drop 4, constructorTagModifier = map toLower} ''D)
$(deriveJSON defaultOptions{fieldLabelModifier = Prelude.drop 4} ''D)

{-
   NOTE:
    ''D is Template Haskell syntax and means a "fully qualified name"

    See page 26 for more Template Haskell transforms I didn't manage to compile (Prelude clashing with NriPrelude)
-}
d :: D Int
d =
    Record
        { testOne = 3.14159
        , testTwo = True
        , testThree = Product "test" 'A' 123
        }