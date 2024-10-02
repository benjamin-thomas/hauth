{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (void)
import Data.Text (Text)

import qualified Data.ByteString.Lazy.Char8 as C8

import qualified Data.Text.IO as TIO
import Network.AMQP
    ( Ack (NoAck)
    , ConnectionOpts
    , DeliveryMode (NonPersistent)
    , Message (msgBody, msgDeliveryMode)
    , QueueOpts (queueAutoDelete, queueDurable, queueName)
    , closeConnection
    , consumeMsgs
    , declareQueue
    , defaultConnectionOpts
    , newMsg
    , newQueue
    , openChannel
    , openConnection''
    , publishMsg
    )

connOpts :: ConnectionOpts
connOpts =
    defaultConnectionOpts

queueName' :: Text
queueName' = "hello-queue"

queue :: QueueOpts
queue =
    newQueue
        { queueName = queueName'
        , queueAutoDelete = False
        , queueDurable = False
        }

-- https://github.com/rabbitmq/rabbitmq-tutorials/blob/main/haskell/send.hs
send :: IO ()
send = do
    conn <- openConnection'' connOpts
    channel <- openChannel conn

    void $ declareQueue channel queue

    void $
        publishMsg
            channel
            ""
            queueName'
            ( newMsg
                { msgBody = "Hello World!"
                , msgDeliveryMode = Just NonPersistent
                }
            )
    TIO.putStrLn " [x] Sent 'Hello World!"
    closeConnection conn

-- https://github.com/rabbitmq/rabbitmq-tutorials/blob/main/haskell/receive.hs
receive :: IO ()
receive = do
    conn <- openConnection'' connOpts
    channel <- openChannel conn
    void $ declareQueue channel queue
    TIO.putStrLn " [*] Waiting for messages. Press any key to exit"
    void $ consumeMsgs channel queueName' NoAck handler
    void getLine -- wait for key press
    closeConnection conn
  where
    handler (msg, _metadata) =
        C8.putStrLn $ " [x] Received: " <> msgBody msg

main :: IO ()
main =
    if False
        then
            send
        else
            receive