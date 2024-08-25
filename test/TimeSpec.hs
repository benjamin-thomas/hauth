module TimeSpec (spec) where

import Data.Time (
    NominalDiffTime,
    TimeLocale (..),
    UTCTime,
    ZonedTime,
    defaultTimeLocale,
    diffUTCTime,
    formatTime,
    fromGregorian,
    hoursToTimeZone,
    parseTimeM,
    utcToZonedTime,
    zonedTimeToUTC,
 )
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)

import Control.Applicative (liftA3)
import Data.Time.Clock (UTCTime (..))
import Data.Time.Format.ISO8601 (
    ISO8601 (iso8601Format),
    formatShow,
 )
import Data.Time.Lens (day, getL, modL, month, setL, year)
import Test.Hspec (Spec, describe, it, shouldBe)

-- rg --files | entr -c cabal test --test-options=--match=/Time/

{-
Bring at least the show instances into scope
>>> import Data.Time ()

>>> t = Data.Time.getCurrentTime
>>> t
2024-08-23 21:25:22.352021709 UTC

>>> Data.Time.Clock.POSIX.utcTimeToPOSIXSeconds <$> t
1724448322.353400674s

>>> Data.Time.Clock.POSIX.utcTimeToPOSIXSeconds <$> Data.Time.getCurrentTime
1724448322.356007317s

>>> Data.Time.Clock.POSIX.posixSecondsToUTCTime 0
1970-01-01 00:00:00 UTC

 -}

-- Having to define my own time locale is wonky.
-- There's probably a better way.
frenchTimeLocale :: TimeLocale
frenchTimeLocale =
    TimeLocale
        { wDays =
            [ ("dimanche", "dim.")
            , ("lundi", "lun.")
            , ("mardi", "mar.")
            , ("mercredi", "mer.")
            , ("jeudi", "jeu.")
            , ("vendredi", "ven.")
            , ("samedi", "sam.")
            ]
        , months =
            [ ("janvier", "janv")
            , ("février", "févr")
            , ("mars", "mars")
            , ("avril", "avr")
            , ("mai", "mai")
            , ("juin", "juin")
            , ("juillet", "juil")
            , ("août", "août")
            , ("septembre", "sept")
            , ("octobre", "oct")
            , ("novembre", "nov")
            , ("décembre", "déc")
            ]
        , amPm = error "AM/PM is not used in French"
        , dateTimeFmt = "%a %e %b %Y %H:%M:%S %Z"
        , dateFmt = "%d/%m/%y"
        , timeFmt = "%H:%M:%S"
        , time12Fmt = "%I:%M:%S %p"
        , knownTimeZones =
            [] -- Not sure how this is meant to work...
        }

spec :: Spec
spec = describe "Time" $ do
    it "returns the epoch" $
        posixSecondsToUTCTime 0 `shouldBe` UTCTime (fromGregorian 1970 1 1) 0

    it "parses dates without a timezone" $
        let parser :: String -> Maybe UTCTime
            parser =
                parseTimeM
                    True
                    defaultTimeLocale
                    "%Y-%m-%d %H:%M:%S"
         in parser
                <$> [ "2024-08-23 21:33:45"
                    , "2024-08-23 21:33:44"
                    , "2024-08-23 21:33:45 UTC"
                    ]
                `shouldBe` [ Just $ posixSecondsToUTCTime 1724448825
                           , Just $ posixSecondsToUTCTime 1724448824
                           , Nothing
                           ]

    {-
        \$ date --utc -Iseconds && date -Iseconds && date +%s
        2024-08-24T05:11:53+00:00
        2024-08-24T07:11:53+02:00
        1724476313

        ---

        NOTE:

        The book makes usage of this function:

            iso8601DateFormat (Just "%H:%M:%S%Q%Z")

        Which is deprecated and points to the module `Data.Time.Format.ISO8601`.
        However all these functions are not meant to output a format string, but rather
        make the format abstract to later callers. So there doesn't seem to be an equivalent
        function to call. So the simplest way is defining a simple string representing
        ISO 8601 format, as in `fmt2`.

        ---

        Prelude Data.Time Data.Time.Format.ISO8601> formatShow iso8601Format <$> getCurrentTime
        "2024-08-24T06:34:12.438629317Z"

        Prelude Data.Time Data.Time.Format.ISO8601> formatShow yearFormat $ 1
        Prelude Data.Time Data.Time.Format.ISO8601> formatShow (calendarFormat BasicFormat) (fromGregorian 2024 8 24)
    "20240824"
         -}
    it "parses dates with a timezone" $
        let
            -- NOTE: there doesn't seem to be a way to capture an error message coming from `parseTimeM`.
            -- Meaning I can't really use `Either String UTCTime` as an output
            parser :: (String, String) -> Maybe UTCTime
            parser (dt, fmt) =
                parseTimeM
                    True
                    defaultTimeLocale
                    fmt
                    dt
            fmt1 = "%Y-%m-%d %H:%M:%S %Z" -- an adhoc format
            fmt2 = "%Y-%m-%dT%H:%M:%S%Z" -- ISO-8601 format
         in
            parser
                <$> [ ("2024-08-23 21:33:45 UTC", fmt1)
                    , ("2024-08-23 23:33:45 +0200", fmt1)
                    , ("2024-08-24T05:11:53+00:00", fmt2)
                    , ("2024-08-24T05:11:53 UTC", fmt2) -- oddly the string UTC is not accepted with fmt2
                    , ("2024-08-24T05:11:53z", fmt2) -- that's the proper (explicit) way to represent a UTC date
                    , ("2024-08-24T07:11:53+02:00", fmt2)
                    ]
                `shouldBe` [ Just (posixSecondsToUTCTime 1724448825) -- fmt1
                           , Just (posixSecondsToUTCTime 1724448825) -- fmt1
                           , Just (posixSecondsToUTCTime 1724476313) -- fmt2
                           , Nothing -- fmt2 (bad trailing UTC)
                           , Just (posixSecondsToUTCTime 1724476313) -- fmt2
                           , Just (posixSecondsToUTCTime 1724476313) -- fmt2
                           ]

    {-
    Prelude Data.Time Data.Time.Format.ISO8601> getCurrentTime
    2024-08-24 06:54:02.612347374 UTC
    Prelude Data.Time Data.Time.Format.ISO8601> formatShow iso8601Format <$> getCurrentTime
    "2024-08-24T06:54:09.756325141Z"
    Prelude Data.Time Data.Time.Format.ISO8601> getZonedTime
    2024-08-24 08:54:29.609313255 CEST
    Prelude Data.Time Data.Time.Format.ISO8601> formatShow iso8601Format <$> getZonedTime
    "2024-08-24T08:54:35.887802722+02:00"
     -}
    it "formats times" $ do
        let epochUtc = posixSecondsToUTCTime 0 :: UTCTime
        let epochFr = utcToZonedTime (hoursToTimeZone 2) (posixSecondsToUTCTime 0) :: ZonedTime
        "1970-01-01T00:00:00Z" `shouldBe` formatShow iso8601Format epochUtc
        "1970-01-01T02:00:00+02:00" `shouldBe` formatShow iso8601Format epochFr
        "1970" `shouldBe` formatTime defaultTimeLocale "%Y" epochUtc
        "00h" `shouldBe` formatTime defaultTimeLocale "%Hh" epochUtc
        "02h" `shouldBe` formatTime defaultTimeLocale "%Hh" epochFr
        "Thursday" `shouldBe` formatTime defaultTimeLocale "%A" epochUtc
        "Thursday" `shouldBe` formatTime defaultTimeLocale "%A" epochFr
        "jeudi" `shouldBe` formatTime frenchTimeLocale "%A" epochFr
        "1970-01-01" `shouldBe` formatTime defaultTimeLocale "%F" epochUtc
        "Thu Jan  1 00:00:00 UTC 1970" `shouldBe` formatTime defaultTimeLocale "%c" epochUtc
        "jeu.  1 janv 1970 00:00:00 UTC" `shouldBe` formatTime frenchTimeLocale "%c" epochUtc
        "jeu.  1 janv 1970 02:00:00 +0200" `shouldBe` formatTime frenchTimeLocale "%c" epochFr

    it "compares times (only UTC)" $ do
        let a = UTCTime (fromGregorian 1970 1 1) 0
        let b = UTCTime (fromGregorian 1970 1 2) 0
        let epochFr = utcToZonedTime (hoursToTimeZone 2) (posixSecondsToUTCTime 0) :: ZonedTime
        a < b `shouldBe` True
        zonedTimeToUTC epochFr == a `shouldBe` True
        diffUTCTime a b `shouldBe` -86400
        diffUTCTime b a `shouldBe` (86400 :: NominalDiffTime) -- seconds
    it "can read or modify time components via lens" $ do
        let almostNextDayUtc = posixSecondsToUTCTime 86300 :: UTCTime
        let almostNextDayFr = utcToZonedTime (hoursToTimeZone 2) almostNextDayUtc :: ZonedTime
        "1970-01-01T23:58:20Z" `shouldBe` formatShow iso8601Format almostNextDayUtc
        "1970-01-02T01:58:20+02:00" `shouldBe` formatShow iso8601Format almostNextDayFr
        "Thursday" `shouldBe` formatTime defaultTimeLocale "%A" almostNextDayUtc
        "Friday" `shouldBe` formatTime defaultTimeLocale "%A" almostNextDayFr
        getL day almostNextDayUtc `shouldBe` 1
        getL day almostNextDayFr `shouldBe` 2
        (getL year almostNextDayFr, getL month almostNextDayFr, getL day almostNextDayFr) `shouldBe` (1970, 1, 2)
        -- let tup3 f g h x = (f x, g x, h x)
        -- let tup3 a b c f = liftA3 (,,) a b c f
        liftA3 (,,) (getL year) (getL month) (getL day) almostNextDayUtc `shouldBe` (1970, 1, 1)
        liftA3 (,,) (getL year) (getL month) (getL day) almostNextDayFr `shouldBe` (1970, 1, 2)
        let newDate = setL month 12 . modL day (+ 30) $ UTCTime (fromGregorian 1970 1 1) 0
        "1970-12-31T00:00:00Z" `shouldBe` formatShow iso8601Format newDate
        let newDate2 = setL month 12 . modL day (+ 31) $ UTCTime (fromGregorian 1970 1 1) 0
        "1970-12-01T00:00:00Z" `shouldBe` formatShow iso8601Format newDate2 -- loops back to 1st day of month
