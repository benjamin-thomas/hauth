module MyLib () where

import NriPrelude (Bool, Char, Float, Int, Maybe (Just, Nothing), Text, (++), (/), (<|), (==), (|>))

import Text (fromInt, isEmpty, repeat, reverse, toInt, trim)

-- import Relude (IO, putStrLn)
-- import Prelude

-- hello :: IO ()
-- hello = putStrLn "Hello, from lib!"

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