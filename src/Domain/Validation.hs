module Domain.Validation (
    validate,
    regexMatch,
    lengthGreaterThan,
    lengthBetween,
    rangeBetween,
) where

import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Text.Regex.PCRE.Heavy (Regex, (=~))

{-
`Validation` is a synonym for function that:
 Receives a raw input and maybe returns an error.
-}
type Validation err raw =
    raw -> Maybe err

-- Original book version below. Simplified a bit further down.
-- validate :: (raw -> valid) -> [Validation err raw] -> raw -> Either [err] valid
-- validate constructor validations val =
--     case concatMap (\f -> maybeToList $ f val) validations of
--         [] -> Right $ constructor val
--         errs -> Left errs

{-

The following expression was condensed:
(\f -> f val)
(\f -> f $ val)
($ val)

>>> validate id [lengthLessThan 5 "nope"] "pa$$"
Left ["nope"]

>>> validate id [lengthLessThan 5 "nope"] "pa$$word"
Right "pa$$word"
 -}
validate :: (raw -> valid) -> [Validation err raw] -> raw -> Either [err] valid
validate constructor validations val =
    case mapMaybe ($ val) validations of
        [] -> Right (constructor val)
        errs -> Left errs

rangeBetween :: (Ord n) => n -> n -> err -> Validation err n
rangeBetween minRange maxRange err val =
    if val >= minRange && val <= maxRange
        then Nothing
        else Just err

{- The book uses the MonoFoldable type class (from the mono-traversable
package), to make the last argument `val` generic.

This would allow the function to work over non-Text types, such as Set, List or
Map, etc. (since they provide a `length` method).

I'll add this later if necessary.
 -}
lengthBetween :: Int -> Int -> err -> Validation err Text
lengthBetween minLen maxLen err val =
    rangeBetween minLen maxLen err (T.length val)

{-
>>> lengthGreaterThan 4 "Too short!" "1234"
Just "Too short!"

>>> lengthGreaterThan 4 "Too short!" "12345"
Nothing
 -}
lengthGreaterThan :: Int -> err -> Validation err Text
lengthGreaterThan n err val =
    if T.length val > n
        then Nothing
        else Just err

regexMatch :: Regex -> err -> Validation err Text
regexMatch regex err val =
    if val =~ regex
        then Nothing
        else Just err
