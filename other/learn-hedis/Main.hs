{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (void)
import Database.Redis

cfg :: ConnectInfo
cfg =
    defaultConnectInfo
        { connectHost = "localhost"
        , connectMaxConnections = 5
        , connectDatabase = 1
        }

main :: IO ()
main = do
    putStrLn "Hi, Redis!"
    conn <- checkedConnect cfg
    world <- runRedis conn $ do
        void $ set "hi" "world"
        get "hi"
    print world