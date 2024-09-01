{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module MyLib (start) where

import qualified Adapter.InMemory.Authentication as M
import Control.Concurrent.STM (TVar, newTVarIO)
import Control.Monad.Reader (
    MonadIO (liftIO),
    MonadReader,
    ReaderT (runReaderT),
 )
import Control.Monad.State (void)
import Domain.Authentication (
    Authentication (Authentication),
    AuthenticationRepo (..),
    EmailVerificationNotif (..),
    SessionRepo (..),
    getUser,
    login,
    mkEmail,
    mkPassword,
    register,
    resolveSessionId,
    verifyEmail,
 )

type State = TVar M.State

newtype App a = App
    {unApp :: ReaderT State IO a}
    deriving (Applicative, Functor, Monad, MonadReader State, MonadIO)

instance AuthenticationRepo App where
    addAuthentication = M.addAuthentication
    setEmailAsVerified = M.setEmailAsVerified
    findUserIdByAuthentication = M.findUserIdByAuthentication
    findEmailFromUserId = M.findEmailFromUserId

instance EmailVerificationNotif App where
    notifyEmailVerification = M.notifyEmailVerification

instance SessionRepo App where
    newSession = M.newSession
    findUserIdBySessionId = M.findUserIdBySessionId

instance MonadFail App where
    fail msg = liftIO (fail msg)

run :: State -> App a -> IO a
run state = flip runReaderT state . unApp

actionsExample :: App ()
actionsExample = do
    let Right email = mkEmail "user@example.com"
        Right password = mkPassword "Hello!123456"
        auth = Authentication email password
    void $ register auth
    Just vCode <- M.getNotificationsForEmail email
    void $ verifyEmail vCode
    Right session <- login auth
    Just uId <- resolveSessionId session
    Just registeredEmail <- getUser uId
    liftIO $ print (session, uId, registeredEmail)

start :: IO ()
start = do
    state <- newTVarIO M.initialState
    run state actionsExample
