{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module ItemRow where

import Data.Text (Text)
import Data.UUID (UUID)
import qualified Data.Vector as V

import Control.Monad (foldM)
import Data.ByteString (ByteString)
import Data.Int (Int32, Int64)
import Data.Profunctor (Profunctor (dimap))
import qualified Data.UUID.V4 as V4
import Data.Vector (Vector)
import Hasql.Connection (Connection)
import qualified Hasql.Decoders as D
import qualified Hasql.Encoders as E
import Hasql.Session (QueryError, run, statement)
import Hasql.Statement (Statement (..))
import Hasql.TH (
    rowsAffectedStatement,
    uncheckedSql,
    uncheckedSqlFile,
    vectorStatement,
 )

data ItemRow = Item
    { itemId :: UUID
    , itemName :: Text
    , itemAltName :: Maybe Text
    , itemIsPhysical :: Bool
    , itemQuantity :: Int32
    }
    deriving (Show)

createItemsTable :: Connection -> IO (Either QueryError ())
createItemsTable =
    run $
        statement () $
            Statement
                createTableSql'
                E.noParams
                D.noResult
                True

-- NOTE: I cannot use resultlessStatement quasi-quote with CREATE TABLE statements.
createTableSql :: ByteString
createTableSql =
    [uncheckedSql|
                CREATE TEMP TABLE items (
                    id UUID NOT NULL PRIMARY KEY,
                    name VARCHAR(100) NOT NULL,
                    alt_name VARCHAR(100) NULL,
                    is_physical BOOLEAN NOT NULL,
                    quantity INT NOT NULL CHECK (quantity >= 0)
                )
            |]

createTableSql' :: ByteString
createTableSql' = [uncheckedSqlFile|other/learn-hasql/sql/create_table_items.sql|]

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

toTup5 :: ItemRow -> (UUID, Text, Maybe Text, Bool, Int32)
toTup5 item = (itemId item, itemName item, itemAltName item, itemIsPhysical item, itemQuantity item)

toTup5b :: ItemRow -> (UUID, Text, Maybe Text, Bool, Int32)
toTup5b Item{..} =
    (itemId, itemName, itemAltName, itemIsPhysical, itemQuantity)

insertItem2 :: Statement ItemRow Int64
insertItem2 =
    dimap
        ( \Item{..} ->
            ( itemId
            , itemName
            , itemAltName
            , itemIsPhysical
            , itemQuantity
            )
        )
        id
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
    c <- V4.nextRandom
    ex <- run (statement (a, "hammer", Just "hand", True, 10) insertItem) conn
    ey <- run (statement (b, "chair", Nothing, True, 99) insertItem) conn
    ez <- run (statement (c, "table", Nothing, False, 1) insertItem) conn
    case (ex, ey, ez) of
        (Right x, Right y, Right z) -> pure $ Right (x + y + z)
        (Left x, _, _) -> pure (Left x)
        (_, Left y, _) -> pure (Left y)
        (_, _, Left z) -> pure (Left z)

doInsertItems2a :: Connection -> IO (Either QueryError Int64)
doInsertItems2a conn = do
    a <- V4.nextRandom
    b <- V4.nextRandom
    c <- V4.nextRandom
    let items =
            [ (a, "hammer", Just "hand", True, 10)
            , (b, "chair", Nothing, True, 99)
            , (c, "table", Nothing, False, 1)
            ]
    foldM
        ( \acc (x, y, z, w, v) ->
            case acc of
                Left x' -> pure $ Left x'
                Right soFar -> do
                    result <- run (statement (x, y, z, w, v) insertItem) conn
                    case result of
                        Left err -> pure $ Left err
                        Right count -> pure $ Right (soFar + count)
        )
        (Right 0)
        items

doInsertItems2b :: Connection -> IO (Either QueryError Int64)
doInsertItems2b conn = do
    a <- V4.nextRandom
    b <- V4.nextRandom
    c <- V4.nextRandom
    let items =
            [ (a, "hammer", Just "hand", True, 10)
            , (b, "chair", Nothing, True, 99)
            , (c, "table", Nothing, False, 1)
            ]
    results <- mapM runInsert items
    pure $ sequence results >>= Right . sum
  where
    -- Insert a single item and return the result
    runInsert :: (UUID, Text, Maybe Text, Bool, Int32) -> IO (Either QueryError Int64)
    runInsert item = run (statement item insertItem) conn

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

doSearchItems :: [Text] -> Connection -> IO (Either QueryError (Vector (UUID, Text, Bool)))
doSearchItems searchTerms = do
    run $ statement (V.fromList searchTerms) searchItems

newtype SearchCriteria = ItemSearchCriteria
    { scNames :: Vector Text
    }

searchItems2_statement :: Statement (Vector Text) (Vector (UUID, Text, Maybe Text, Bool, Int32))
searchItems2_statement =
    [vectorStatement|
            SELECT id          :: uuid
                 , name        :: text
                 , alt_name    :: text?
                 , is_physical :: bool
                 , quantity    :: int
            FROM
               items
            WHERE
               name = ANY($1 :: text[])
        |]

uncurry5 :: (a -> b -> c -> d -> e -> f) -> (a, b, c, d, e) -> f
uncurry5 f (a, b, c, d, e) = f a b c d e

searchItems2 :: Statement SearchCriteria (Vector ItemRow)
searchItems2 =
    dimap
        toInput
        toOutput
        searchItems2_statement
  where
    -- I could just use `id` and the underlying type instead (instead of `scNames`)
    toInput :: SearchCriteria -> Vector Text
    toInput = scNames

    -- toItem :: (UUID, Text, Maybe Text, Bool, Int32) -> ItemRow
    -- toItem (uuid, name, altName, isPhysical, quantity) =
    --     Item
    --         { itemId = uuid
    --         , itemName = name
    --         , itemAltName = altName
    --         , itemIsPhysical = isPhysical
    --         , itemQuantity = quantity
    --         }

    toItem' :: (UUID, Text, Maybe Text, Bool, Int32) -> ItemRow
    toItem' = uncurry5 Item

    toOutput :: Vector (UUID, Text, Maybe Text, Bool, Int32) -> Vector ItemRow
    toOutput = fmap toItem'

doSearchItems2 :: [Text] -> Connection -> IO (Either QueryError (Vector ItemRow))
doSearchItems2 searchTerms =
    run $
        statement
            (ItemSearchCriteria (V.fromList searchTerms))
            searchItems2
