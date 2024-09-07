{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Arrow (left)
import Control.Monad.Except (MonadError, liftEither, runExceptT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C8
import Data.Functor.Contravariant (
    Contravariant (contramap),
    Predicate (Predicate, getPredicate),
    (>$<),
 )
import Data.Vector (Vector)
import Hasql.Connection
import Hasql.Session
import ItemRow
import UserRow

isEven :: Predicate Int
isEven = Predicate even

getConnection :: IO (Either ConnectionError Connection)
getConnection =
    acquire (C8.pack "dbname=hauth")

data AppError
    = ConnErr (Maybe ByteString)
    | TableCreationError QueryError
    | InsertUsersErr QueryError
    | InsertItemsErr QueryError
    | FetchUsersErr QueryError
    | FetchItemsErr QueryError
    deriving (Show)

orStop :: (MonadError e' m, MonadIO m) => IO (Either e a) -> (e -> e') -> m a
orStop r l = do
    liftEither =<< liftIO (left l <$> r)

usersWork :: (MonadError AppError m, MonadIO m) => m [UserRow]
usersWork = do
    conn <- getConnection `orStop` ConnErr
    () <- createUsersTable conn `orStop` TableCreationError
    n <- doInsertUsers conn `orStop` InsertUsersErr
    liftIO $ putStrLn $ "[usersWork] Inserted " <> show n <> " users"
    listUsers conn `orStop` FetchUsersErr

itemsWork :: (MonadError AppError m, MonadIO m) => m (Vector ItemRow)
itemsWork = do
    conn <- getConnection `orStop` ConnErr
    () <- createItemsTable conn `orStop` TableCreationError
    n <- doInsertItems conn `orStop` InsertItemsErr
    liftIO $ putStrLn $ "[itemsWork] Inserted " <> show n <> " items"
    doSearchItems2 conn `orStop` FetchItemsErr

main :: IO ()
main =
    do
        putStrLn "---"
        p "A1" (getPredicate (contramap length isEven) "abc")
        p "B1" (getPredicate (contramap length isEven) "abcd")

        putStrLn "---"
        p "A2" (getPredicate (length >$< isEven) "abc")
        p "B2" (getPredicate (length >$< isEven) "abcd")

        putStrLn "---"
        p "A3" (even . length $ "abc")
        p "B3" (even . length $ "abcd")

        putStrLn ""
        putStrLn "~~~~~~~~~~~~~~~~~~~~~~~~~~"
        putStrLn "~~~ Doing users stuff! ~~~"
        putStrLn "~~~~~~~~~~~~~~~~~~~~~~~~~~"
        result <- runExceptT usersWork
        case result of
            Left err -> putStrLn $ "[usersWork] Error: " <> show err
            Right users -> do
                putStrLn "[usersWork] Success!"
                mapM_ print users

        putStrLn ""
        putStrLn "~~~~~~~~~~~~~~~~~~~~~~~~~~"
        putStrLn "~~~ Doing items stuff! ~~~"
        putStrLn "~~~~~~~~~~~~~~~~~~~~~~~~~~"
        result2 <- runExceptT itemsWork
        case result2 of
            Left err -> putStrLn $ "[itemsWork] Error: " <> show err
            Right items -> do
                putStrLn "[itemsWork] Success!"
                mapM_ print items

        putStrLn "DONE!"
  where
    p x v = putStrLn $ x <> ": " <> show v
