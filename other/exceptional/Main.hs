{-# HLINT ignore "Avoid restricted function" #-}
{-# HLINT ignore "Use lambda-case" #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Main where

import Control.Exception.Safe
import Data.Function ((&))
import System.Environment.Blank (getEnv)
import Text.Read (readMaybe)

{-

Chapter 2 : Exception Handling
==============================

cabal run exceptional
 -}

isBelow10 :: Int -> Either String ()
isBelow10 x =
    if x < 10
        then Right ()
        else
            Left
                ( -- this is an "impure" exception (not referentially transparent)
                  error "above 10!"
                )

-- | check1 wont't actually throw because the error value is never evaluated!
check1 :: IO ()
check1 =
    case isBelow10 20 of
        Left _ -> putStrLn "isBelow10 returned False"
        Right () -> putStrLn "isBelow10 returned successfully"

check2 :: IO ()
check2 =
    case isBelow10 20 of
        Left e -> do
            -- This is a bit "backwards", due to lazy evaluation
            -- When using eager evaluation (as in most languages), the line just below would never print!
            putStrLn "isBelow10 returned False"
            putStrLn "Now, I will evaluate `e` and thus fail!"
            putStrLn e
        Right () -> putStrLn "isBelow10 returned successfully"

data ServerException
    = ServerOnFireException
    | ServerNotPluggedInException
    deriving (Show)

instance Exception ServerException

data MyException
    = ThisException
    | ThatException
    deriving (Show)

instance Exception MyException

impureExceptions :: IO ()
impureExceptions = do
    putStrLn "==> [impureExceptions] enter..."
    check1
    check2
    putStrLn "==> [impureExceptions] exit!"

safeExceptions1 :: IO ()
safeExceptions1 = do
    putStrLn "==> [safeExceptions1] enter..."
    throw ServerOnFireException
        `catch` (\e -> putStrLn $ "--> Caught: " <> show (e :: ServerException))
    putStrLn "==> [safeExceptions1] exit!"

safeExceptions2 :: IO ()
safeExceptions2 = do
    putStrLn "==> [safeExceptions2] enter..."
    -- `handle` is `catch` flipped
    handle
        (\e -> putStrLn $ "--> Caught: " <> show (e :: ServerException))
        (throw ServerOnFireException)
    putStrLn "==> [safeExceptions2] exit!"

getRunActionEnv :: IO (Maybe Int)
getRunActionEnv =
    fmap
        (>>= readMaybe)
        (getEnv "ACTION")

{- FOURMOLU_DISABLE -}
{-
*Main> run $ throw ServerOnFireException
EXN! ServerException: ServerOnFireException

*Main> run $ throw ThatException
EXN! MyException: ThatException

*Main> run $ throwString "wat"
EXN! SomeException: Control.Exception.Safe.throwString called with:

wat
Called from:
  throwString (<interactive>:5:7 in interactive:Ghci3)
 -}
{- FOURMOLU_ENABLE -}
run :: IO () -> IO ()
run action =
    action
        `catch` (\e -> putStrLn $ "EXN! ServerException: " <> show (e :: ServerException))
        `catch` (\e -> putStrLn $ "EXN! MyException: " <> show (e :: MyException))
        `catchAny` (\e -> putStrLn $ "EXN! SomeException: " <> show e)

run2a :: IO () -> IO ()
run2a action =
    handle (\e -> putStrLn $ "EXN! ServerException: " <> show (e :: ServerException)) $
        handle (\e -> putStrLn $ "EXN! MyException: " <> show (e :: MyException)) $
            handleAny (\e -> putStrLn $ "EXN! SomeException: " <> show e) action

{- FOURMOLU_DISABLE -}
run2b :: IO () -> IO ()
run2b action =
      handle (\e -> putStrLn $ "EXN! ServerException: " <> show (e :: ServerException))
    $ handle (\e -> putStrLn $ "EXN! MyException: " <> show (e :: MyException))
    $ handleAny (\e -> putStrLn $ "EXN! SomeException: " <> show e) action
{- FOURMOLU_ENABLE -}

run2c :: IO () -> IO ()
run2c =
    handle (\e -> putStrLn $ "EXN! ServerException: " <> show (e :: ServerException))
        . handle (\e -> putStrLn $ "EXN! MyException: " <> show (e :: MyException))
        . handleAny (\e -> putStrLn $ "EXN! SomeException: " <> show e)

{- FOURMOLU_DISABLE -}
run2d :: IO () -> IO ()
run2d =
    handle (\e -> putStrLn $ "EXN! ServerException: " <> show (e :: ServerException))
  . handle (\e -> putStrLn $ "EXN! MyException: " <> show (e :: MyException))
  . handleAny (\e -> putStrLn $ "EXN! SomeException: " <> show e)
{- FOURMOLU_ENABLEi -}

run3 :: IO () -> IO ()
run3 action =
    action
        -- We can "pipe" those functions in any order!
        & handleAny (\e -> putStrLn $ "EXN! SomeException: " <> show e)
        & handle (\e -> putStrLn $ "EXN! MyException: " <> show (e :: MyException))
        & handle (\e -> putStrLn $ "EXN! ServerException: " <> show (e :: ServerException))

{-
ACTION=2 cabal run exceptional
cabal repl exceptional
./manage/cabal_repl exceptional
 -}
main :: IO ()
main = do
    runAction <- getRunActionEnv
    case runAction of
        Just 1 -> impureExceptions
        Just 2 -> safeExceptions1
        Just 3 -> safeExceptions2
        Nothing -> error "Required env var: ACTION=NUMBER"
        Just n -> error $ "Unknown ACTION: " <> show n
