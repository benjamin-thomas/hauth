{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module ItemRow where

import Control.Monad (void)
import Data.Text (Text)
import Data.UUID (UUID)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ (sql)
import GHC.Generics (Generic)

createItemsTable :: Connection -> IO ()
createItemsTable conn = void $ execute_ conn qry
 where
  qry =
    [sql|CREATE TEMP TABLE
               items ( id UUID NOT NULL PRIMARY KEY
                     , name VARCHAR(100) NOT NULL
                     , alt_name VARCHAR(100) NULL
                     , is_physical BOOLEAN NOT NULL
                     , quantity INT NOT NULL CHECK (quantity >= 0)
                     )
        |]

data Item = Item
  { itemId :: UUID
  , itemName :: Text
  , itemAltName :: Maybe Text
  , itemIsPhysical :: Bool
  , itemQuantity :: Int
  }
  deriving (Generic, ToRow, FromRow, Show)

insertItems :: Connection -> [Item] -> IO ()
insertItems conn item = void $ executeMany conn qry item
 where
  qry =
    [sql|INSERT INTO
                items ( id
                      , name
                      , alt_name
                      , is_physical
                      , quantity
                      )
               VALUES (?,?,?,?,?)
        |]

listItems :: Connection -> IO [Item]
listItems conn = query_ conn "SELECT * FROM items"