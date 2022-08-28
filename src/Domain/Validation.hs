module Domain.Validation where

import Data.Maybe (maybeToList)
import Data.Text as T (Text, length)
import Text.Regex.PCRE.Heavy (Regex, (=~))

{-
`Validation` is a synonym for function that receives any input `a` and returns
a `Maybe` of any `error` message, otherwise `Nothing` if the input is valid.
-}
type Validation error a = a -> Maybe error

validate :: (a -> b) -> [Validation error a] -> a -> Either [error] b
validate constructor validations val =
    case concatMap (\f -> maybeToList $ f val) validations of
        [] -> Right $ constructor val
        errs -> Left errs

rangeBetween :: Int -> Int -> error -> Validation error Int
rangeBetween minRange maxRange error_ val =
    if val >= minRange && val <= maxRange
        then Nothing
        else Just error_

lengthBetween :: Int -> Int -> error -> Validation error Text
lengthBetween minLen maxLen error_ val =
    rangeBetween minLen maxLen error_ (T.length val)

regexMatch :: Regex -> error -> Validation error Text
regexMatch regex error_ val =
    if val =~ regex
        then Nothing
        else Just error_