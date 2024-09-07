{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module ItemRow where

import Data.Text (Text)
import Data.UUID (UUID)
import qualified Data.Vector as V

import Data.Int
import Data.Profunctor
import qualified Data.UUID.V4 as V4
import Data.Vector (Vector)
import Hasql.Connection (Connection)
import qualified Hasql.Decoders as D
import qualified Hasql.Encoders as E
import Hasql.Session (QueryError, run, statement)
import Hasql.Statement (Statement (..))
import Hasql.TH (rowsAffectedStatement, vectorStatement)

data ItemRow = Item
    { itemId :: UUID
    , itemName :: Text
    , itemAltName :: Maybe Text
    , itemIsPhysical :: Bool
    , itemQuantity :: Int
    }
    deriving (Show)

createItemsTable :: Connection -> IO (Either QueryError ())
createItemsTable =
    run $
        statement () $
            Statement
                "CREATE TEMP TABLE items ( id UUID NOT NULL PRIMARY KEY, name VARCHAR(100) NOT NULL , alt_name VARCHAR(100) NULL , is_physical BOOLEAN NOT NULL , quantity INT NOT NULL CHECK (quantity >= 0))"
                E.noParams
                D.noResult
                True

insertItem :: Statement (UUID, Text, Maybe Text, Bool, Int32) Int64
insertItem =
    [rowsAffectedStatement|
    INSERT INTO
        items ( id
              , name
              , alt_name
              , is_physical
              , quantity
              )
       VALUES ( $1 :: uuid
              , $2 :: text
              , $3 :: text?
              , $4 :: bool
              , $5 :: int
              )|]

doInsertItems :: Connection -> IO (Either QueryError Int64)
doInsertItems conn = do
    a <- V4.nextRandom
    b <- V4.nextRandom
    ex <- run (statement (a, "hammer", Just "hand", True, 10) insertItem) conn
    ey <- run (statement (b, "chair", Nothing, True, 99) insertItem) conn
    case (ex, ey) of
        (Right x, Right y) -> pure $ Right (x + y)
        (Left x, _) -> pure (Left x)
        (_, Left y) -> pure (Left y)

searchItems :: Statement (Vector Text) (Vector (UUID, Text, Bool))
searchItems =
    [vectorStatement|
        SELECT id          :: uuid
             , name        :: text
             , is_physical :: bool
         FROM
           items
        WHERE
          name = ANY($1 :: text[])
    |]

doSearchItems :: Connection -> IO (Either QueryError (Vector (UUID, Text, Bool)))
doSearchItems = do
    run $ statement (V.fromList ["hammer", "chair"]) searchItems

newtype ItemSearchCriteria = ItemSearchCriteria
    { names :: Vector Text
    }

searchItems2_statement :: Statement (Vector Text) (Vector (UUID, Text, Bool))
searchItems2_statement =
    [vectorStatement|
            SELECT id          :: uuid
                 , name        :: text
                 , is_physical :: bool
            FROM
               items
            WHERE
               name = ANY($1 :: text[])
        |]

-- Refactored searchItems using dimap
searchItems2 :: Statement ItemSearchCriteria (Vector ItemRow)
searchItems2 =
    dimap
        toInput -- Transform input
        toOutput -- Transform output
        searchItems2_statement
  where
    -- I could just use `id` and the underlying type instead (instead of `names`)
    toInput :: ItemSearchCriteria -> Vector Text
    toInput = names

    toItem :: (UUID, Text, Bool) -> ItemRow
    toItem (uuid, name, isPhysical) =
        Item
            { itemId = uuid
            , itemName = name
            , itemAltName = Nothing -- You'll need to handle this case if you need it
            , itemIsPhysical = isPhysical
            , itemQuantity = 0 -- You can replace this with a real value
            }

    toOutput :: Vector (UUID, Text, Bool) -> Vector ItemRow
    toOutput = fmap toItem

doSearchItems2 :: Connection -> IO (Either QueryError (Vector ItemRow))
doSearchItems2 =
    run $
        statement
            (ItemSearchCriteria (V.fromList ["hammer", "chair"]))
            searchItems2
