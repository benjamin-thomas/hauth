{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Adapter.PostgreSQL_Simple.Authentication where

import Control.Exception (bracket)
import Control.Exception.Safe (MonadThrow, throwString, try)
import Control.Monad.Reader
import Data.Bifunctor
import Data.ByteString
import Data.Has (Has (getter))
import Data.Pool
import Data.Text (Text)
import Data.Time (NominalDiffTime)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ (sql)
import qualified Domain.Authentication as D
import Text.StringRandom

type PG r m =
    ( Has (Pool Connection) r
    , MonadReader r m
    , MonadIO m
    , MonadThrow m
    )

withConn :: (PG r m) => (Connection -> IO a) -> m a
withConn action = do
    pool <- asks getter
    liftIO . withResource pool $ action

type State = Pool Connection

data Config = Config
    { cfgConnStr :: ByteString -- Simply use "dbname=hauth" for dev env
    , cfgStripeCount :: Int
    , cfgMaxOpenPerStripe :: Int
    , cfgIdleConnTimeout :: NominalDiffTime
    }

withPool :: Config -> (State -> IO a) -> IO a
withPool cfg =
    bracket initPool destroyPool
  where
    initPool :: IO (Pool Connection)
    initPool = newPool poolCfg

    destroyPool :: Pool a -> IO ()
    destroyPool = destroyAllResources

    poolCfg :: PoolConfig Connection
    poolCfg = defaultPoolConfig onCreate onDestroy ttlMs maxOpenCount
      where
        onCreate :: IO Connection
        onCreate = connectPostgreSQL (cfgConnStr cfg)

        onDestroy :: Connection -> IO ()
        onDestroy = close

        ttlMs :: Double
        ttlMs = 10000

        maxOpenCount :: Int
        maxOpenCount = 10

-- https://www.postgresql.org/docs/current/errcodes-appendix.html
pgUniqueKeyViolation :: SqlError -> Bool
pgUniqueKeyViolation = (== "23505") . sqlState

-- FIXME: Remove those runtime exceptions, they don't make sens to me (breaks referential transparency).
-- FIXME: The book argues that they are errors "not meant to happen" and as such, we don't want to handle them.
-- FIXME: I agree with the first throwString (fail loudly at dev time), but the last one bothers me.
-- FIXME: I'll keep reading for now, and see how it goes. The caller is not forced to check the possible
-- FIXME: exception, that's what bothers me. Maybe using the suffix `exn` would be good enough.
addAuthentication ::
    (PG r m) =>
    D.Authentication ->
    m (Either D.RegistrationError (D.UserId, D.VerificationCode))
addAuthentication (D.Authentication email password) = do
    let rawEmail = D.rawEmail email
        rawPassword = D.rawPassword password
    rawVCode <- liftIO $ stringRandomIO "[A-Za-z0-9]{64}"
    let vCode = D.MkVerificationCode rawVCode
    result <- withConn $ \conn ->
        try $ query conn insertUserQuery (rawEmail, rawPassword, rawVCode)
    case result of
        Right [Only userId] -> do
            let userId' = D.MkUserId userId
            pure $ Right (userId', vCode)
        Right _ -> throwString "Should never happen: postgres did not return a single user id"
        Left err@SqlError{sqlErrorMsg = msg} ->
            if pgUniqueKeyViolation err && "users_email_key" `isInfixOf` msg
                then pure $ Left D.RegistrationErrorEmailTaken
                else throwString $ "Should never happen: " <> show err
  where
    insertUserQuery :: Query
    insertUserQuery =
        [sql|
            INSERT INTO users
            ( email
            , pw_hash
            , verification_code
            )
            VALUES
            ( ?
            , crypt(?, gen_salt('bf'))
            , ?
            )
            RETURNING user_id
        |]

unsafeMkEmail :: (MonadThrow m) => Text -> m D.Email
unsafeMkEmail userEmail =
    case D.mkEmail userEmail of
        Left _ -> throwString "Should never happen: invalid email is in the DB"
        Right email -> pure email

setEmailAsVerified :: (PG r m) => D.VerificationCode -> m (Either D.EmailVerificationError (D.UserId, D.Email))
setEmailAsVerified (D.MkVerificationCode rawVCode) = do
    result <- withConn $ \conn ->
        try $ query conn setEmailAsVerifiedQuery (Only rawVCode)
    case result of
        Left err@SqlError{} -> throwString $ "SQL error: " <> show err
        Right [(userId, userEmail)] -> do
            userEmail' <- unsafeMkEmail userEmail
            let userId' = D.MkUserId userId
            pure $ Right (userId', userEmail')
        Right _ -> pure $ Left D.InvalidEmailVerificationCodeError
  where
    setEmailAsVerifiedQuery :: Query
    setEmailAsVerifiedQuery =
        [sql|
            UPDATE users
            SET verified_at = current_timestamp
            WHERE verification_code = ?
            RETURNING user_id, email::TEXT
        |]

authenticate ::
    (Has (Pool Connection) r, MonadReader r m, MonadIO m, MonadThrow m) =>
    Text ->
    Text ->
    m (Maybe D.UserId)
authenticate email password = do
    result <- withConn $ \conn ->
        query conn findUserQuery (email, password)
    case result of
        [Only userId] -> pure $ Just $ D.MkUserId userId
        _ -> pure Nothing
  where
    findUserQuery :: Query
    findUserQuery =
        [sql|
            SELECT user_id
            FROM users
            WHERE email = ? AND pw_hash = crypt(?, pw_hash)
        |]

findEmailFromUserId :: (PG r m) => D.UserId -> m (Maybe D.Email)
findEmailFromUserId (D.MkUserId userId) = do
    result <- withConn $ \conn ->
        query conn findEmailQuery (Only userId)
    case result of
        [Only email] -> do
            Just <$> unsafeMkEmail email
        _ -> pure Nothing
  where
    findEmailQuery :: Query
    findEmailQuery =
        [sql|
            SELECT email::TEXT
            FROM users
            WHERE user_id = ?
        |]

findUserIdByAuthentication :: (PG r m) => D.Authentication -> m (Maybe (D.UserId, Bool)) -- Bool says if the email has been verified
findUserIdByAuthentication auth = do
    result <- withConn $ \conn ->
        query
            conn
            findUserIdQuery
            ( D.rawEmail . D.authEmail $ auth
            , D.rawPassword . D.authPassword $ auth
            )
    case result of
        [(userId, isVerified)] -> do
            let userId' = D.MkUserId userId
            pure $ Just (userId', isVerified)
        _ -> pure Nothing
  where
    findUserIdQuery :: Query
    findUserIdQuery =
        [sql|
            SELECT user_id
                 , verified_at IS NOT NULL AS is_verified
            FROM users
            WHERE email = ? AND pw_hash = crypt(?, pw_hash)
        |]

-- TODO: remove later
devPoolCfg :: PoolConfig Connection
devPoolCfg = defaultPoolConfig onCreate onDestroy ttlMs maxOpenCount
  where
    onCreate :: IO Connection
    onCreate = connectPostgreSQL "dbname=hauth"

    onDestroy :: Connection -> IO ()
    onDestroy = close

    ttlMs :: Double
    ttlMs = 10000

    maxOpenCount :: Int
    maxOpenCount = 10

initialState :: PoolConfig Connection -> IO (Pool Connection)
initialState =
    newPool

{- FOURMOLU_DISABLE -}
{- HLINT ignore "Avoid restricted function" -}
testInsert :: Text -> IO (Either D.RegistrationError (D.UserId, D.VerificationCode))
testInsert name = do
    (st :: State) <- newPool devPoolCfg
    flip runReaderT st $ addAuthentication auth
  where
    auth :: D.Authentication
    auth =
        -- `Bifunctor.first` is "mapError"
        either (error . show) id $
            D.Authentication
                <$> first (const badEmail) (D.mkEmail $ name <> "@example.org")
                <*> first (const badPassword) (D.mkPassword "Hello!123456")
      where
        (badEmail, badPassword) = ("Bad email", "Bad password") :: (Text, Text)
{- FOURMOLU_ENABLE -}

testVerify :: Text -> IO (Either D.EmailVerificationError (D.UserId, D.Email))
testVerify code =
    newPool devPoolCfg
        >>= runReaderT (setEmailAsVerified (D.MkVerificationCode code))

testAuthenticate :: Text -> Text -> IO (Maybe D.UserId)
testAuthenticate email password =
    newPool devPoolCfg
        >>= runReaderT (authenticate email password)

testFindEmailFromUserId :: Int -> IO (Maybe D.Email)
testFindEmailFromUserId userId =
    newPool devPoolCfg
        >>= runReaderT (findEmailFromUserId (D.MkUserId userId))