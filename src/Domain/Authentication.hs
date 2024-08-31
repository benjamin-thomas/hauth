module Domain.Authentication (
    -- * Types
    Auth (..),
    Email,
    mkEmail,
    rawEmail,
    Password,
    mkPassword,
    rawPassword,
    UserId,
    SessionId,
    EmailValidationError (..),
    PasswordValidationError (..),
    EmailVerificationError (..),

    -- * Ports
    AuthRepo (..),
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

type VerificationCode = Text

data EmailVerificationError
    = InvalidEmailVerificationCodeError
    deriving (Show, Eq)

class (Monad m) => AuthRepo m where
    addAuth :: Auth -> m (Either RegistrationError VerificationCode)
    setEmailAsVerified :: VerificationCode -> m (Either EmailVerificationError ())
    findUserByAuth :: Auth -> m (Maybe (UserId, Bool)) -- Bool says if the email has been verified
    findEmailFromUserId :: UserId -> m (Maybe Email)

getUser :: (AuthRepo m) => UserId -> m (Maybe Email)
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
register :: (AuthRepo m, EmailVerificationNotif m) => Auth -> m (Either RegistrationError ())
register auth = runExceptT $ do
    vCode <- ExceptT $ addAuth auth
    let email = authEmail auth
    lift $ notifyEmailVerification email vCode

-- TEMP IMPLEMENTATIONS

instance AuthRepo IO where
    addAuth (Auth email _pass) = do
        TIO.putStrLn $ "adding auth: " <> rawEmail email
        pure $ Right "fake verification code"
    setEmailAsVerified _vCode = do
        pure $ Left InvalidEmailVerificationCodeError

fakeAuth =
    let Right email = mkEmail "user@example.com"
        Right password = mkPassword "123456789Ab"
     in Auth email password

-- verifyCode :: AuthRepo m => VerificationCode -> m (Either EmailVerificationError ())
-- verifyCode = setEmailAsVerified

verifyEmail :: (AuthRepo m) => VerificationCode -> m (Either EmailVerificationError ())
verifyEmail = setEmailAsVerified

instance EmailVerificationNotif IO where
    notifyEmailVerification email vCode =
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

-- newtype UserId = UserId Int

newtype SessionId = SessionId Text

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
