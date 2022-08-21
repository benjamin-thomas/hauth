module MyLib (hello) where

import NriPrelude (Bool, Char, Float, Int, List, Maybe (Just, Nothing), Result, Text, (++), (/), (<|), (==), (|>))
import qualified Relude (Text)

import Result (Result (Err, Ok), withDefault)
import Text (fromInt, isEmpty, repeat, reverse, toInt, toList, trim)

-- import Relude (IO, putStrLn)
-- import Prelude

{-
Pass strings to `Main.hs`:

    Prelude.putStrLn hello
    Relude.putStrLn hello
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
intStr = fromInt 1234

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