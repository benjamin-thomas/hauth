{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module UserRow where

import Control.Monad (void)
import Data.Text (Text)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ (sql)
import GHC.Generics (Generic)

createUsersTable :: Connection -> IO ()
createUsersTable conn = void $ execute_ conn qry
 where
  qry =
    [sql|CREATE TEMP TABLE
               users ( id INT NOT NULL PRIMARY KEY GENERATED ALWAYS AS IDENTITY
                     , email VARCHAR(20) NOT NULL CHECK (TRIM(email) <> '')
                     , is_active BOOLEAN NOT NULL
                     )
        |]

data User = User
  { userId :: Int
  , userEmail :: Text
  , userIsActive :: Bool
  }
  deriving (Generic, ToRow, FromRow, Show)

insertUsers :: Connection -> [User] -> IO ()
insertUsers conn user = void $ executeMany conn qry (map toRow user)
 where
  qry =
    [sql|INSERT INTO
                users (is_active, email)
               VALUES (        ?,     ?)
        |]
  toRow User{..} = (userIsActive, userEmail) -- insert less columns or order independent

listUsers :: Connection -> IO [User]
listUsers conn = query_ conn "SELECT * FROM users"