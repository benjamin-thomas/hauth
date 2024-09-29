{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Adapter.Redis.Authentication
    ( State
    , newSession
    , findUserIdBySessionId
    , initialStateDev
    ) where

import Control.Arrow (left)
import Control.Exception.Safe (MonadThrow, throwString)
import Control.Monad ((<=<))
import Control.Monad.Reader
    ( MonadIO (liftIO)
    , MonadReader
    , asks
    )
import Data.ByteString (ByteString)
import Data.Has (Has (getter))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8', encodeUtf8)
import Data.Text.Encoding.Error (UnicodeException)
import Database.Redis (ConnectInfo (..))
import qualified Database.Redis as R
import qualified Domain.Authentication as D
import qualified Text.Read as TR
import Text.StringRandom (stringRandomIO)

type State = R.ConnectInfo

initialStateDev :: R.ConnectInfo
initialStateDev =
    R.defaultConnectInfo
        { connectHost = "localhost"
        , connectMaxConnections = 5
        , connectDatabase = 1
        }

type Redis r m =
    ( Has State r
    , MonadReader r m
    , MonadIO m
    , MonadThrow m
    )

tshow :: (Show a) => a -> Text
tshow = T.pack . show

newSession :: (Redis r m) => D.UserId -> m D.SessionId
newSession (D.MkUserId userId) = do
    connInfo <- asks getter
    sessionId <- liftIO $ stringRandomIO "[A-Za-z0-9]{32}"
    result <- liftIO $ R.withCheckedConnect connInfo $ do
        flip
            R.runRedis
            ( R.set
                (encodeUtf8 sessionId)
                (encodeUtf8 $ tshow userId)
            )

    case result of
        Right R.Ok -> pure (D.MkSessionId sessionId)
        unexpected ->
            throwString $
                "Unexpected redis response: " <> show unexpected

data ResultError
    = RedisError R.Reply
    | DecodeError UnicodeException
    | NoBytes
    | ExpectedInt String
    deriving (Show)

findUserIdBySessionId :: (Redis r m) => D.SessionId -> m (Maybe D.UserId)
findUserIdBySessionId (D.MkSessionId sessionId) = do
    connInfo <- asks getter
    result <-
        liftIO $
            R.withCheckedConnect connInfo $
                flip
                    R.runRedis
                    (R.get $ encodeUtf8 sessionId)
    case handleResult result of
        Left NoBytes -> pure Nothing
        Left err -> throwString $ show err
        Right userId -> pure $ Just userId
  where
    handleResult :: Either R.Reply (Maybe ByteString) -> Either ResultError D.UserId
    handleResult =
        (Right . D.MkUserId)
            <=< (hoistMaybe (ExpectedInt "userId") . parseUserId)
            <=< (left DecodeError . decodeUtf8')
            <=< hoistMaybe NoBytes
            <=< left RedisError
      where
        hoistMaybe :: e -> Maybe a -> Either e a
        hoistMaybe err = maybe (Left err) Right

        parseUserId :: Text -> Maybe Int
        parseUserId = TR.readMaybe . T.unpack
