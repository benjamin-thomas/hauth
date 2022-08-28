module Domain.Authentication (
    mkEmail,
    mkPassword,
    Email (Email),
    EmailValidationError (..),
    PasswordValidationError (..),
) where

import Data.Text (Text)
import Domain.Validation (lengthBetween, regexMatch, validate)
import Text.Regex.PCRE.Heavy (re)

data Auth = Auth
    { authEmail :: Email
    , authPassword :: Password
    }
    deriving (Show, Eq)

data RegistrationError
    = RegistrationErrorEmailTaken
    deriving (Show, Eq)

-- VALIDATION ERRORS

data EmailValidationError
    = InvalidEmailErr
    deriving (Show, Eq)

data PasswordValidationError
    = PasswordTooShortError
    | PasswordMustContainUpperCaseError
    | PasswordMustContainLowerCaseError
    | PasswordMustContainNumberError
    deriving (Show, Eq)

-- EMAIL

newtype Email = Email {emailRaw :: Text} deriving (Show, Eq)

rawEmail :: Email -> Text
rawEmail = emailRaw

mkEmail :: Text -> Either [EmailValidationError] Email
mkEmail =
    validate
        Email
        -- Better to let in a possibly wrong email address, rather than block a possibly correct email address
        [ regexMatch [re|@.+\..+|] InvalidEmailErr
        ]

-- PASSWORD

newtype Password = Password {passwordRaw :: Text} deriving (Show, Eq)

rawPassword :: Password -> Text
rawPassword = passwordRaw

mkPassword :: Text -> Either [PasswordValidationError] Password
mkPassword =
    validate
        Password
        [ lengthBetween 10 500 PasswordTooShortError
        , regexMatch [re|\d|] PasswordMustContainNumberError
        , regexMatch [re|[a-z]|] PasswordMustContainLowerCaseError
        , regexMatch [re|[A-Z]|] PasswordMustContainUpperCaseError
        ]
