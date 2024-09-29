{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

module MyLib (start) where

import qualified Adapter.InMemory.Authentication as M
import qualified Adapter.PostgreSQL_Simple.Authentication as PG
import qualified Adapter.Redis.Authentication as Redis

import Control.Concurrent.STM (TVar, newTVarIO)

import Control.Exception.Safe (MonadThrow, bracket, throwString)
import Control.Monad.Reader
    ( MonadIO (liftIO)
    , MonadReader
    , ReaderT (runReaderT)
    )
import Domain.Authentication
    ( Authentication (Authentication)
    , AuthenticationRepo (..)
    , EmailVerificationNotif (..)
    , SessionRepo (..)
    , getUser
    , login
    , mkEmail
    , mkPassword
    , register
    , resolveSessionId
    , verifyEmail
    )
import GHC.Stack (HasCallStack)
import Katip
    ( ColorStrategy (ColorIfTerminal)
    , Katip
    , KatipContext
    , KatipContextT
    , LogEnv
    , Severity (InfoS)
    , Verbosity (V2)
    , closeScribes
    , defaultScribeSettings
    , initLogEnv
    , mkHandleScribe
    , permitItem
    , registerScribe
    , runKatipContextT
    )
import System.IO (stdout)
import Text.StringRandom (stringRandomIO)

type AppState = (PG.State, Redis.State, TVar M.State)

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
    findUserIdByAuthentication = PG.findUserIdByAuthentication
    findEmailFromUserId = PG.findEmailFromUserId

instance EmailVerificationNotif App where
    notifyEmailVerification = M.notifyEmailVerification

instance SessionRepo App where
    newSession = Redis.newSession
    findUserIdBySessionId = Redis.findUserIdBySessionId

instance MonadFail App where
    fail msg = liftIO (fail msg)

run :: LogEnv -> AppState -> App a -> IO a
run logEnv appState = runKatipContextT logEnv () mempty . flip runReaderT appState . unApp

{- HLINT ignore "Avoid restricted function" -}
actionsExample :: App ()
actionsExample = do
    randStr <- liftIO $ stringRandomIO "[A-Za-z0-9]{8}"
    let email = either (error "Invalid email") id $ mkEmail $ mconcat ["user-", randStr, "@example.com"]
        password = either (error "Invalid password") id $ mkPassword "Hello!123456"
        auth = Authentication email password
    authResult <- register auth
    either (throwString . show) pure authResult
    maybeCode <- M.getNotificationsForEmail email
    vCode <- maybe (throwString "no vCode") pure maybeCode
    verifyResult <- verifyEmail vCode
    either (throwString . show) pure verifyResult
    loginResult <- login auth
    case loginResult of
        Left x -> liftIO . putStrLn $ "Failed to login: " <> show x
        Right sessionId -> do
            Just uId <- resolveSessionId sessionId
            Just registeredEmail <- getUser uId
            liftIO $ putStrLn $ "(TEMP) ok session:" <> show (sessionId, uId, registeredEmail)

withLogEnv :: (LogEnv -> IO a) -> IO a
withLogEnv = bracket createLogEnv closeScribes
  where
    createLogEnv = do
        logEnv <- initLogEnv "HAuth" "prod"
        stdoutScribe <- mkHandleScribe ColorIfTerminal stdout (permitItem InfoS) V2
        registerScribe "stdout" stdoutScribe defaultScribeSettings logEnv

{-

To delete all redis keys, use:

redis-cli -n 1 --scan --pattern '*' -i 0.01 | xargs redis-cli -n 1 unlink

 -}
start :: (HasCallStack) => IO ()
start = withLogEnv $ \logEnv -> do
    authState <- newTVarIO M.initialState
    poolConn <- PG.initialState PG.devPoolCfg
    let redisSession = Redis.initialStateDev
    run logEnv (poolConn, redisSession, authState) actionsExample
