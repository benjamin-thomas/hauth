{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module MyLib (start) where

import qualified Adapter.InMemory.Authentication as M
import qualified Adapter.PostgreSQL_Simple.Authentication as PG

import Control.Concurrent.STM (TVar, newTVarIO)

import Control.Exception.Safe (MonadThrow, bracket)
import Control.Monad.Reader (
    MonadIO (liftIO),
    MonadReader,
    ReaderT (runReaderT),
    void,
 )
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
import Katip (
    ColorStrategy (ColorIfTerminal),
    Katip,
    KatipContext,
    KatipContextT,
    LogEnv,
    Severity (InfoS),
    Verbosity (V2),
    closeScribes,
    defaultScribeSettings,
    initLogEnv,
    mkHandleScribe,
    permitItem,
    registerScribe,
    runKatipContextT,
 )
import System.IO (stdout)

type AppState = (PG.State, TVar M.State)

newtype App a = App
    {unApp :: ReaderT AppState (KatipContextT IO) a}
    deriving
        ( Applicative
        , Functor
        , Monad
        , MonadReader AppState
        , MonadThrow
        , MonadIO
        , KatipContext
        , Katip
        )

instance AuthenticationRepo App where
    addAuthentication = PG.addAuthentication
    setEmailAsVerified = PG.setEmailAsVerified
    findUserIdByAuthentication = M.findUserIdByAuthentication
    findEmailFromUserId = PG.findEmailFromUserId

instance EmailVerificationNotif App where
    notifyEmailVerification = M.notifyEmailVerification

instance SessionRepo App where
    newSession = M.newSession
    findUserIdBySessionId = M.findUserIdBySessionId

instance MonadFail App where
    fail msg = liftIO (fail msg)

run :: LogEnv -> AppState -> App a -> IO a
run logEnv appState = runKatipContextT logEnv () mempty . flip runReaderT appState . unApp

{- HLINT ignore "Avoid restricted function" -}
actionsExample :: App ()
actionsExample = do
    let email = either (error "Invalid email") id $ mkEmail "user@example.com"
        password = either (error "Invalid password") id $ mkPassword "Hello!123456"
        auth = Authentication email password
    void $ register auth
    Just vCode <- M.getNotificationsForEmail email -- FIXME! memory impl is not in syn with pg impl
    void $ verifyEmail vCode
    loginResult <- login auth
    case loginResult of
        Left x -> liftIO . putStrLn $ "Failed to login: " <> show x
        Right sessionId -> do
            Just uId <- resolveSessionId sessionId
            Just registeredEmail <- getUser uId
            liftIO $ putStrLn "Logged in successfully"
            liftIO $ print (sessionId, uId, registeredEmail)

withLogEnv :: (LogEnv -> IO a) -> IO a
withLogEnv = bracket createLogEnv closeScribes
  where
    createLogEnv = do
        logEnv <- initLogEnv "HAuth" "prod"
        stdoutScribe <- mkHandleScribe ColorIfTerminal stdout (permitItem InfoS) V2
        registerScribe "stdout" stdoutScribe defaultScribeSettings logEnv

start :: IO ()
start = withLogEnv $ \logEnv -> do
    authState <- newTVarIO M.initialState
    poolConn <- PG.initialState PG.devPoolCfg
    run logEnv (poolConn, authState) actionsExample
