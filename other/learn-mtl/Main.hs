{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.Except (
    MonadError (throwError),
    MonadTrans (lift),
 )
import Control.Monad.Reader (
    MonadReader (ask),
    ReaderT (runReaderT),
    asks,
 )
import Data.Text (Text)

type Env = [(Text, Int)] -- (variableName, variableValue)

data Expr
    = Val Int
    | Add Expr Expr
    | Var Text
    deriving (Show)

eval :: Expr -> ReaderT Env Maybe Int
eval = \case
    Val n -> pure n
    Add x y -> (+) <$> eval x <*> eval y
    Var str -> do
        env <- ask
        lift (lookup str env)

{-
>>> example1
Just 8
 -}
example1 :: Maybe Int
example1 =
    runReaderT
        (eval (Add (Val 1) (Add (Var "x") (Var "y"))))
        [("x", 2), ("y", 5)]

example2 :: Maybe Int
example2 =
    runReaderT
        (eval (Add (Var "x") (Var "y")))
        []

example3 :: Maybe Int
example3 =
    runReaderT
        (eval (Add (Val 2) (Val 3)))
        []

newtype Env2
    = Env2 {unEnv :: [(Text, Int)]}

newtype EvalError
    = VarNotFound Text
    deriving (Show)

eval2 :: (MonadReader Env2 m, MonadError EvalError m) => Expr -> m Int
eval2 = \case
    Val n -> pure n
    Add x y -> (+) <$> eval2 x <*> eval2 y
    Var str -> do
        env <- asks unEnv
        case lookup str env of
            Just n -> pure n
            Nothing -> throwError $ VarNotFound str

{-
>>> example4
Right 7
 -}
example4 :: Either EvalError Int
example4 =
    runReaderT
        (eval2 (Add (Var "x") (Var "y")))
        (Env2 [("x", 2), ("y", 5)])

{-
>>> example5
Left (VarNotFound "y")
 -}
example5 :: Either EvalError Int
example5 =
    runReaderT
        (eval2 (Add (Var "x") (Var "y")))
        (Env2 [("x", 2)])

main :: IO ()
main = putStrLn "Hello learn-mtl"
