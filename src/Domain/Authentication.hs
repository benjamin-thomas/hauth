module Domain.Authentication (
    mkEmail,
    mkPassword,
    rawPassword,
    Email (Email),
    EmailValidationError (..),
    PasswordValidationError (..),
) where

import Control.Monad.Except (ExceptT (ExceptT), MonadError (throwError), MonadTrans (lift), runExceptT)
import Data.Text (Text, unpack)
import Domain.Validation (lengthLessThan, regexMatch, validate)
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
        [ lengthLessThan 10 PasswordTooShortError
        , regexMatch [re|\d|] PasswordMustContainNumberError
        , regexMatch [re|[a-z]|] PasswordMustContainLowerCaseError
        , regexMatch [re|[A-Z]|] PasswordMustContainUpperCaseError
        ]

-- TEMP: I'm not sure where the code below should go yet

-- RUNTIME AUTHENTICATION

type VerificationCode = Text

data EmailVerificationError
    = InvalidEmailVerificationCodeError
    deriving (Show, Eq)

class (Monad m) => AuthRepo m where
    addAuth :: Auth -> m (Either RegistrationError VerificationCode)
    setEmailAsVerified :: VerificationCode -> m (Either EmailVerificationError ())
    findUserByAuth :: Auth -> m (Maybe (UserId, Bool))

class (Monad m) => EmailVerificationNotif m where
    notifyEmailVerification :: Email -> VerificationCode -> m ()

class (Monad m) => SessionRepo m where
    newSession :: UserId -> m SessionId
    findUserIdBySessionId :: SessionId -> m (Maybe UserId)

-- TEMP IMPLEMENTATIONS

instance AuthRepo IO where
    addAuth (Auth email _pass) = do
        putStrLn $ "adding auth: " <> unpack (rawEmail email)
        return $ Right "fake verification code"
    setEmailAsVerified _vCode = do
        return $ Left InvalidEmailVerificationCodeError

register :: (AuthRepo m, EmailVerificationNotif m) => Auth -> m (Either RegistrationError ())
register auth = runExceptT $ do
    vCode <- ExceptT $ addAuth auth
    let email = authEmail auth
    lift $ notifyEmailVerification email vCode

-- verifyCode :: AuthRepo m => VerificationCode -> m (Either EmailVerificationError ())
-- verifyCode = setEmailAsVerified

verifyEmail :: (AuthRepo m) => VerificationCode -> m (Either EmailVerificationError ())
verifyEmail = setEmailAsVerified

instance EmailVerificationNotif IO where
    notifyEmailVerification email vCode =
        putStrLn $ "Notify " <> unpack (rawEmail email) <> " - " <> unpack vCode

{-
Testing temporary impls in the REPL

cabal repl>:l Domain.Authentication
cabal repl>:l ./src/Domain/Authentication.hs

-- copy/paste below
let Right email = mkEmail "user@example.com"
let Right password = mkPassword "123456789Ab"
let auth = Auth email password
register auth
verifyEmail $ rawEmail email

\*Domain.Authentication> let Right email = mkEmail "user@example.com"
\*Domain.Authentication> let Right password = mkPassword "123456789Ab"
\*Domain.Authentication> let auth = Auth email password
\*Domain.Authentication> register auth
adding auth: user@example.com
Notify user@example.com - fake verification code
Right ()

-- Not implemented yet
\*Domain.Authentication> verifyEmail "user@example.com"
Left InvalidEmailVerificationCodeError

\*Domain.Authentication> verifyEmail $ rawEmail email
Left InvalidEmailVerificationCodeError

-}

type UserId = Int

-- newtype UserId = UserId Int

type SessionId = Text

data LoginError
    = InvalidCredentialsError
    | EmailNotVerifiedError
    deriving (Show, Eq)

login :: (AuthRepo m, SessionRepo m) => Auth -> m (Either LoginError SessionId)
login auth = runExceptT $ do
    result <- lift $ findUserByAuth auth
    case result of
        Nothing -> throwError InvalidCredentialsError
        Just (_, False) -> throwError EmailNotVerifiedError
        Just (uId, _) -> lift $ newSession uId

resolveSessionId :: (SessionRepo m) => SessionId -> m (Maybe UserId)
resolveSessionId = findUserIdBySessionId