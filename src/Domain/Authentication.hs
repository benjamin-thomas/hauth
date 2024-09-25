module Domain.Authentication (
    -- * Types
    Authentication (..),
    UserId (..),
    SessionId (..),
    EmailValidationError (..),
    PasswordValidationError (..),
    EmailVerificationError (..),
    RegistrationError (..),
    VerificationCode (..),

    -- * Types and their special constructors
    Email,
    mkEmail,
    rawEmail,
    Password,
    mkPassword,
    rawPassword,

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
import Domain.Validation (lengthGreaterThan, regexMatch, validate)
import Katip (
    KatipContext,
    Severity (InfoS),
    katipAddContext,
    logTM,
    ls,
    sl,
 )
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
    = MkVerificationCode Text
    deriving (Show, Eq, Ord)

data EmailVerificationError
    = InvalidEmailVerificationCodeError
    deriving (Show, Eq)

class (Monad m) => AuthenticationRepo m where
    addAuthentication :: Authentication -> m (Either RegistrationError (UserId, VerificationCode))
    setEmailAsVerified :: VerificationCode -> m (Either EmailVerificationError (UserId, Email))
    findUserIdByAuthentication :: Authentication -> m (Maybe (UserId, Bool)) -- Bool says if the email has been verified
    findEmailFromUserId :: UserId -> m (Maybe Email)

getUser :: (AuthenticationRepo m) => UserId -> m (Maybe Email)
getUser = findEmailFromUserId

class (Monad m) => EmailVerificationNotif m where
    notifyEmailVerification :: Email -> VerificationCode -> m ()

class (Monad m) => SessionRepo m where
    newSession :: UserId -> m SessionId
    findUserIdBySessionId :: SessionId -> m (Maybe UserId)

withUserIdContext :: (KatipContext m) => UserId -> m a -> m a
withUserIdContext (MkUserId userId) = katipAddContext (sl "userId" userId)

{-
Using `ExceptT` allows us to short circuit in case `addAuth` returns a `Left`.
`runExceptT` converts an `ExceptT` int an `Either`.

`ExceptT` comes from the `mtl` package.
 -}
register ::
    ( KatipContext m
    , AuthenticationRepo m
    , EmailVerificationNotif m
    ) =>
    Authentication ->
    m (Either RegistrationError ())
register auth = runExceptT $ do
    (userId, vCode) <- ExceptT $ addAuthentication auth
    let email = authEmail auth
    lift $ notifyEmailVerification email vCode
    withUserIdContext userId $
        $(logTM) InfoS $
            ls (rawEmail email) <> " registered successfully"

verifyEmail ::
    ( KatipContext m
    , AuthenticationRepo m
    ) =>
    VerificationCode ->
    m (Either EmailVerificationError ())
verifyEmail vCode = runExceptT $ do
    (userId, email) <- ExceptT $ setEmailAsVerified vCode
    withUserIdContext userId $
        $(logTM) InfoS $
            ls (rawEmail email) <> " has been verified successfully"

newtype UserId = MkUserId Int
    deriving (Show, Eq)

newtype SessionId = MkSessionId Text
    deriving (Show, Eq, Ord)

data LoginError
    = InvalidCredentialsError
    | EmailNotVerifiedError
    deriving (Show, Eq)

login ::
    ( KatipContext m
    , AuthenticationRepo m
    , SessionRepo m
    ) =>
    Authentication ->
    m (Either LoginError SessionId)
login auth = runExceptT $ do
    result <- lift $ findUserIdByAuthentication auth
    case result of
        Nothing -> throwError InvalidCredentialsError
        Just (_, False) -> throwError EmailNotVerifiedError
        Just (userId, _) ->
            withUserIdContext userId . lift $ do
                sessionId <- newSession userId
                $(logTM) InfoS $
                    ls (rawEmail $ authEmail auth) <> " logged in successfully"
                pure sessionId

resolveSessionId :: (SessionRepo m) => SessionId -> m (Maybe UserId)
resolveSessionId = findUserIdBySessionId
