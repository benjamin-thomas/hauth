{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}

module Adapter.InMemory.Authentication where

import Control.Concurrent.STM (
    TVar,
    atomically,
    modifyTVar,
    newTVarIO,
    readTVar,
    readTVarIO,
    writeTVar,
 )
import Control.Monad.Except (
    MonadError (throwError),
    MonadIO (..),
    MonadTrans (lift),
    runExceptT,
    when,
 )
import Control.Monad.Reader (MonadReader, ReaderT (runReaderT), asks)
import Data.Bifunctor (second)
import Data.Has (Has (getter))
import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Domain.Authentication as D
import Text.StringRandom (stringRandomIO)

data State = State
    { stateAuthentications :: [(D.UserId, D.Authentication)]
    , stateUnverifiedEmails :: Map D.VerificationCode D.Email
    , stateVerifiedEmails :: Set D.Email
    , stateUserIdCounter :: Int
    , stateNotifications :: Map D.Email D.VerificationCode
    , stateSessions :: Map D.SessionId D.UserId
    }
    deriving (Show, Eq)

initialState :: State
initialState =
    State
        { stateAuthentications = []
        , stateUnverifiedEmails = mempty
        , stateVerifiedEmails = mempty
        , stateUserIdCounter = 0
        , stateNotifications = mempty
        , stateSessions = mempty
        }

type InMemory r m =
    ( Has (TVar State) r
    , MonadReader r m
    , MonadIO m
    )

-- findUserIdBySessionId :: SessionId -> m (Maybe UserId)
findUserIdBySessionId :: (InMemory r m) => D.SessionId -> m (Maybe D.UserId)
findUserIdBySessionId sessionId = do
    tvar <- asks getter
    liftIO $
        fmap
            (\(st :: State) -> Map.lookup sessionId $ stateSessions st)
            (readTVarIO tvar)

newSessionId :: D.UserId -> IO D.SessionId
newSessionId userId = do
    let userId' = T.pack . show $ D.unUserId userId
    fmap
        ( \randStr ->
            D.mkSessionId $
                mconcat
                    [ userId'
                    , "::"
                    , randStr
                    ]
        )
        (stringRandomIO "[A-Za-z0-9]{16}")

newSession :: (InMemory r m) => D.UserId -> m D.SessionId
newSession userId = do
    sessionId <- liftIO $ newSessionId userId
    (tvar :: TVar State) <- asks getter
    liftIO . atomically $ do
        st <- readTVar tvar
        let sessions = stateSessions st
            newSessions = Map.insert sessionId userId sessions
            newState = st{stateSessions = newSessions}
        writeTVar tvar newState
        pure sessionId

-- notifyEmailVerification :: Email -> VerificationCode -> m ()
-- notifyEmailVerification :: (InMemory r m) => D.Email -> D.VerificationCode -> m ()
-- notifyEmailVerification email vCode = do
--     (tvar :: TVar State) <- asks getter
--     liftIO . atomically $ do
--         st <- readTVar tvar
--         writeTVar tvar $
--             st
--                 { stateNotifications = Map.insert email vCode (stateNotifications st)
--                 }

notifyEmailVerification :: (InMemory r m) => D.Email -> D.VerificationCode -> m ()
notifyEmailVerification email vCode = do
    (tvar :: TVar State) <- asks getter
    liftIO $ atomically $ modifyTVar tvar $ \st ->
        st
            { stateNotifications = Map.insert email vCode (stateNotifications st)
            }

-- The following function is only useful for testing purposes
getNotificationsForEmail :: (InMemory r m) => D.Email -> m (Maybe D.VerificationCode)
getNotificationsForEmail email = do
    tvar <- asks getter
    liftIO $
        fmap
            (Map.lookup email . stateNotifications)
            (readTVarIO tvar)

-- findEmailFromUserId :: (InMemory r m) => D.UserId -> m (Maybe D.Email)
-- findEmailFromUserId userId = do
--     tvar <- asks getter
--     st <- liftIO $ readTVarIO tvar
--     let findMay = List.find ((== userId) . fst) (stateAuthentications st)
--     pure $ D.authEmail . snd <$> findMay

findEmailFromUserId :: (InMemory r m) => D.UserId -> m (Maybe D.Email)
findEmailFromUserId userId = do
    tvar <- asks getter
    liftIO $
        fmap
            ( \st ->
                let findMay = List.find ((== userId) . fst) (stateAuthentications st)
                 in (D.authEmail . snd <$> findMay)
            )
            (readTVarIO tvar)

findUserIdByAuthentication :: (InMemory r m) => D.Authentication -> m (Maybe (D.UserId, Bool)) -- Bool says if the email has been verified
findUserIdByAuthentication auth = do
    tvar <- asks getter
    liftIO $
        fmap
            ( \(st :: State) ->
                let authMay = List.find ((auth ==) . snd) (stateAuthentications st)
                 in setVerified (stateVerifiedEmails st) <$> authMay
            )
            (readTVarIO tvar)
  where
    setVerified :: Set D.Email -> (D.UserId, D.Authentication) -> (D.UserId, Bool)
    setVerified emails =
        second
            ( \auth' ->
                Set.member
                    (D.authEmail auth')
                    emails
            )

-- setEmailAsVerified :: VerificationCode -> m (Either EmailVerificationError ())
setEmailAsVerified :: (InMemory r m) => D.VerificationCode -> m (Either D.EmailVerificationError ())
setEmailAsVerified vCode = do
    tvar <- asks getter
    liftIO . atomically $ do
        st <- readTVar tvar
        case Map.lookup vCode (stateUnverifiedEmails st) of
            Nothing ->
                pure $ Left D.InvalidEmailVerificationCodeError
            Just email -> do
                let newUnverifiedEmails = Map.delete vCode $ stateUnverifiedEmails st
                    newVerifiedEmails = Set.insert email $ stateVerifiedEmails st
                    newState =
                        st
                            { stateUnverifiedEmails = newUnverifiedEmails
                            , stateVerifiedEmails = newVerifiedEmails
                            }
                writeTVar tvar newState
                pure $ Right ()

-- version from the book
setEmailAsVerified' :: (InMemory r m) => D.VerificationCode -> m (Either D.EmailVerificationError ())
setEmailAsVerified' vCode = do
    (tvar :: TVar State) <- asks getter
    liftIO . atomically . runExceptT $ do
        st <- lift $ readTVar tvar
        let unverifiedEmails = stateUnverifiedEmails st
            verifiedEmails = stateVerifiedEmails st
            emailMay = Map.lookup vCode unverifiedEmails
        case emailMay of
            Nothing ->
                throwError D.InvalidEmailVerificationCodeError
            Just email -> do
                let newUnverifiedEmails = Map.delete vCode unverifiedEmails
                    newVerifiedEmails = Set.insert email verifiedEmails
                    newState =
                        st
                            { stateUnverifiedEmails = newUnverifiedEmails
                            , stateVerifiedEmails = newVerifiedEmails
                            }
                lift $ writeTVar tvar newState

-- addAuthentication :: Authentication -> m (Either RegistrationError VerificationCode)
addAuthentication :: (InMemory r m) => D.Authentication -> m (Either D.RegistrationError D.VerificationCode)
addAuthentication auth = do
    (tvar :: TVar State) <- asks getter
    randStr <- liftIO $ stringRandomIO "[A-Za-z0-9]{16}"
    liftIO . atomically $ do
        st <- readTVar tvar
        let authMay =
                List.find
                    (\x -> D.authEmail auth == D.authEmail x)
                    (map snd $ stateAuthentications st)
        case authMay of
            Just _ ->
                pure $ Left D.RegistrationErrorEmailTaken
            Nothing -> do
                let vCode = D.mkVerificationCode randStr
                    newStateUserIdCounter = 1 + stateUserIdCounter st
                    newAuth = (D.mkUserId newStateUserIdCounter, auth)
                    newAuthentications = newAuth : stateAuthentications st
                    newUnverifiedEmails =
                        Map.insert
                            vCode
                            (D.authEmail auth)
                            (stateUnverifiedEmails st)
                    newState =
                        st
                            { stateUserIdCounter = newStateUserIdCounter
                            , stateAuthentications = newAuthentications
                            , stateUnverifiedEmails = newUnverifiedEmails
                            }
                writeTVar tvar newState
                pure $ Right vCode

-- version from the book
addAuthentication' :: (InMemory r m) => D.Authentication -> m (Either D.RegistrationError D.VerificationCode)
addAuthentication' auth = do
    tvar <- asks getter
    vCode <- liftIO $ D.mkVerificationCode <$> stringRandomIO "[A-Za-z0-9]{16}"
    liftIO . atomically . runExceptT $ do
        st <- lift $ readTVar tvar
        let authentications = stateAuthentications st
            email = D.authEmail auth
            isDuplicate = elem email . map (D.authEmail . snd) $ authentications
        when isDuplicate $ throwError D.RegistrationErrorEmailTaken
        let newUserId = stateUserIdCounter st + 1
            newAuthentications = (D.mkUserId newUserId, auth) : authentications
            unverifiedEmails = stateUnverifiedEmails st
            newUnverifiedEmails = Map.insert vCode email unverifiedEmails
            newState =
                st
                    { stateUserIdCounter = newUserId
                    , stateAuthentications = newAuthentications
                    , stateUnverifiedEmails = newUnverifiedEmails
                    }
        lift $ writeTVar tvar newState
        pure vCode

testRun :: IO (Either D.RegistrationError D.VerificationCode)
testRun = do
    s <- newTVarIO initialState
    flip runReaderT s $ addAuthentication auth
  where
    auth =
        let Right email = D.mkEmail "user@example.com"
            Right password = D.mkPassword "Hello!123456"
         in D.Authentication email password

{- FOURMOLU_DISABLE -}
{-

*MyLib> :load Adapter.InMemory.Authentication
*Adapter.InMemory.Authentication> testRun
Right (VerificationCode "{V_TOKEN_1}")

-- To run the commands below, run the above command step by step (to have all the variables into scope)

*Adapter.InMemory.Authentication> flip runReaderT s $ findEmailFromUserId (D.mkUserId 1)
Just (Email {emailRaw = "user@example.com"})

*Adapter.InMemory.Authentication> flip runReaderT s $ findUserIdByAuthentication auth
Just (UserId 1,False)

*Adapter.InMemory.Authentication> flip runReaderT s $ newSession (D.mkUserId 1)
SessionId "1::{S_TOKEN_1}"

*Adapter.InMemory.Authentication> flip runReaderT s $ newSession (D.mkUserId 1)
SessionId "1::{S_TOKEN_2}"

*Adapter.InMemory.Authentication> flip runReaderT s $ findUserIdBySessionId (D.mkSessionId "1::{S_TOKEN_1}")
Just (UserId 1)

*Adapter.InMemory.Authentication> flip runReaderT s $ setEmailAsVerified (D.mkVerificationCode "{V_TOKEN_1}")
Right ()

*Adapter.InMemory.Authentication> flip runReaderT s $ findUserIdByAuthentication auth
Just (UserId 1,True)

 -}