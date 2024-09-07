{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Control.Exception (bracket)
import Data.ByteString (ByteString)
import Data.Pool

import qualified Data.UUID.V4 as V4
import Database.PostgreSQL.Simple
import ItemRow
import UserRow

{-
NOTE:

Specifying an empty connection string is probably what I'll want most times.

I can easily inject the usual connection params via env vars, like so:
PGDATABASE=hauth ./manage/cabal_repl learn-postgresql-simple

For simplicity here, I'm hardcoding the dbname.
See the README to understand how this "peer" authentication works
-}

connStr :: ByteString
connStr = "dbname=hauth"

-- Alternative to the similar library function `withConnect`, which requires a
-- ConnectInfo record, which I don't want to use here.
withConn :: (Connection -> IO a) -> IO a
withConn =
    bracket
        (connectPostgreSQL connStr)
        close

withPool :: (Pool Connection -> IO a) -> IO a
withPool =
    bracket
        (newPool cfg)
        destroyAllResources
  where
    cfg :: PoolConfig Connection
    cfg = defaultPoolConfig whenCreate whenDestroy ttlMs maxOpenCount

    whenCreate :: IO Connection
    whenCreate = connectPostgreSQL connStr

    whenDestroy :: Connection -> IO ()
    whenDestroy = close

    ttlMs :: Double
    ttlMs = 10000

    maxOpenCount :: Int
    maxOpenCount = 10

withPoolConn :: (Connection -> IO a) -> IO a
withPoolConn f =
    withPool $ \pool -> withResource pool $ \conn -> f conn

fetchOneCol :: Connection -> IO [Only Int]
fetchOneCol conn = query_ conn "SELECT 1+2"

fetchTwoCol :: Connection -> IO [(Int, Int)]
fetchTwoCol conn = query_ conn "SELECT 1+2, 3+4"

-- cabal run learn-postgresql-simple
main :: IO ()
main =
    do
        withConn $ \conn -> do
            putStrLn "\n=== Fetching via a single connection ==="

            a <- fetchOneCol conn
            putStrLn $ "Fetching 1 column: " <> show a

            b <- fetchTwoCol conn
            putStrLn $ "Fetching 2 columns: " <> show b

        withPoolConn $ \conn -> do
            putStrLn "\n=== Fetching via a pooled connection ==="

            a <- fetchOneCol conn
            putStrLn $ "Fetching 1 column: " <> show a

            b <- fetchTwoCol conn
            putStrLn $ "Fetching 2 columns: " <> show b

        putStrLn "\n=== Now let's test some table constraints ==="

        withConn $ \conn -> do
            putStrLn "Creating temp tables..."
            createUsersTable conn
            createItemsTable conn

            putStrLn "Inserting users..."
            insertUsers
                conn
                [ User
                    { userId = 0
                    , userEmail = "john@example"
                    , userIsActive = True
                    }
                , User
                    { userId = 0
                    , userEmail = "jane@example"
                    , userIsActive = False
                    }
                ]

            putStrLn "\n=== Listing users ==="
            users <- listUsers conn
            mapM_ print users

            putStrLn "Inserting items..."
            a <- V4.nextRandom
            b <- V4.nextRandom
            insertItems
                conn
                [ Item
                    { itemId = a
                    , itemName = "foo"
                    , itemAltName = Nothing
                    , itemIsPhysical = True
                    , itemQuantity = 1
                    }
                , Item
                    { itemId = b
                    , itemName = "bar"
                    , itemAltName = Just "baz"
                    , itemIsPhysical = False
                    , itemQuantity = 2
                    }
                ]

            putStrLn "\n=== Listing items ==="
            items <- listItems conn
            mapM_ print items
