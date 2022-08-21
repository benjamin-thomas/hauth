module MyLib (hello) where

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

import NriPrelude (Bool (True), Char, Float, Int, List, Maybe (Just, Nothing), Result, Text, (*), (++), (-), (/), (<), (<|), (==), (>), (|>))

import Prelude (IO)

import Data.Either (Either (Left, Right))

import Result (Result (Err, Ok), withDefault)
import Text (fromInt, isEmpty, repeat, reverse, toInt, toList, trim)

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