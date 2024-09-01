module Domain.Authentication (
    -- * Types
    Authentication (..),
    Email,
    mkEmail,
    rawEmail,
    Password,
    mkPassword,
    rawPassword,
    UserId,
    mkUserId,
    unUserId,
    SessionId,
    mkSessionId,
    EmailValidationError (..),
    PasswordValidationError (..),
    EmailVerificationError (..),
    RegistrationError (..),
    VerificationCode,
    mkVerificationCode,

    -- * Ports
    AuthenticationRepo (..),
    EmailVerificationNotif (..),
    SessionRepo (..),

    -- * Use cases
    register,
    verifyEmail,
    login,
    resolveSessionId,
    getUser,
) where

import Control.Monad.Except (ExceptT (ExceptT), MonadError (throwError), MonadTrans (lift), runExceptT)
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import Domain.Validation (lengthGreaterThan, regexMatch, validate)
import Text.Regex.PCRE.Heavy (re)

data Authentication = Authentication
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

newtype Email = Email {emailRaw :: Text}
    deriving (Show, Eq, Ord)

rawEmail :: Email -> Text
rawEmail = emailRaw

{- | Public constructor for the internal `Email` type. AKA a "smart constructor".
>>> mkEmail "bad@input"
Left [InvalidEmailErr]

>>> mkEmail "user@example.com"
Right (Email {emailRaw = "user@example.com"})
-}
mkEmail :: Text -> Either [EmailValidationError] Email
mkEmail =
    validate
        Email
        -- Better to let in a possibly wrong email address, rather than block a
        -- possibly correct email address
        [ regexMatch [re|@.+\..+|] InvalidEmailErr
        ]

-- PASSWORD

newtype Password = Password {passwordRaw :: Text} deriving (Show, Eq)

rawPassword :: Password -> Text
rawPassword = passwordRaw

{-
>>> mkPassword "abc"
Left [PasswordTooShortError,PasswordMustContainNumberError,PasswordMustContainUpperCaseError]

>>> mkPassword "123456789Ab"
Right (Password {passwordRaw = "123456789Ab"})
 -}
mkPassword :: Text -> Either [PasswordValidationError] Password
mkPassword =
    validate
        Password
        [ lengthGreaterThan 10 PasswordTooShortError
        , regexMatch [re|\d|] PasswordMustContainNumberError
        , regexMatch [re|[a-z]|] PasswordMustContainLowerCaseError
        , regexMatch [re|[A-Z]|] PasswordMustContainUpperCaseError
        ]

-- TEMP: I'm not sure where the code below should go yet

-- RUNTIME AUTHENTICATION

newtype VerificationCode
    = VerificationCode Text
    deriving (Show, Eq, Ord)

mkVerificationCode :: Text -> VerificationCode
mkVerificationCode = VerificationCode

data EmailVerificationError
    = InvalidEmailVerificationCodeError
    deriving (Show, Eq)

class (Monad m) => AuthenticationRepo m where
    addAuthentication :: Authentication -> m (Either RegistrationError VerificationCode)
    setEmailAsVerified :: VerificationCode -> m (Either EmailVerificationError ())
    findUserIdByAuthentication :: Authentication -> m (Maybe (UserId, Bool)) -- Bool says if the email has been verified
    findEmailFromUserId :: UserId -> m (Maybe Email)

getUser :: (AuthenticationRepo m) => UserId -> m (Maybe Email)
getUser = findEmailFromUserId

class (Monad m) => EmailVerificationNotif m where
    notifyEmailVerification :: Email -> VerificationCode -> m ()

class (Monad m) => SessionRepo m where
    newSession :: UserId -> m SessionId
    findUserIdBySessionId :: SessionId -> m (Maybe UserId)

{-
Using `ExceptT` allows us to short circuit in case `addAuth` returns a `Left`.
`runExceptT` converts an `ExceptT` int an `Either`.

`ExceptT` comes from the `mtl` package.
 -}
register :: (AuthenticationRepo m, EmailVerificationNotif m) => Authentication -> m (Either RegistrationError ())
register auth = runExceptT $ do
    vCode <- ExceptT $ addAuthentication auth
    let email = authEmail auth
    lift $ notifyEmailVerification email vCode

-- TEMP IMPLEMENTATIONS

instance AuthenticationRepo IO where
    addAuthentication (Authentication email _pass) = do
        TIO.putStrLn $ "adding auth: " <> rawEmail email
        pure $ Right (VerificationCode "fake verification code")
    setEmailAsVerified _vCode = do
        pure $ Left InvalidEmailVerificationCodeError

-- verifyCode :: AuthRepo m => VerificationCode -> m (Either EmailVerificationError ())
-- verifyCode = setEmailAsVerified

verifyEmail :: (AuthenticationRepo m) => VerificationCode -> m (Either EmailVerificationError ())
verifyEmail = setEmailAsVerified

instance EmailVerificationNotif IO where
    notifyEmailVerification email (VerificationCode vCode) =
        TIO.putStrLn $ "Notify " <> rawEmail email <> " - " <> vCode

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

newtype UserId = UserId Int
    deriving (Show, Eq)

mkUserId :: Int -> UserId
mkUserId = UserId

unUserId :: UserId -> Int
unUserId (UserId n) = n

newtype SessionId = SessionId Text
    deriving (Show, Eq, Ord)

mkSessionId :: Text -> SessionId
mkSessionId = SessionId

data LoginError
    = InvalidCredentialsError
    | EmailNotVerifiedError
    deriving (Show, Eq)

login :: (AuthenticationRepo m, SessionRepo m) => Authentication -> m (Either LoginError SessionId)
login auth = runExceptT $ do
    result <- lift $ findUserIdByAuthentication auth
    case result of
        Nothing -> throwError InvalidCredentialsError
        Just (_, False) -> throwError EmailNotVerifiedError
        Just (uId, _) -> lift $ newSession uId

resolveSessionId :: (SessionRepo m) => SessionId -> m (Maybe UserId)
resolveSessionId = findUserIdBySessionId
