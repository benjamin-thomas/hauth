{-

cabal run learn-stm

cabal repl learn-stm
./manage/cabal_repl learn-stm

==========================================

\*Main> :main
First operation
Result 1: 100
---
Second operation
Result 2: 201

 -}

module Main (main) where

import System.Environment.Blank (getEnv)
import System.Exit (exitFailure)
import Text.Read (readMaybe)
import qualified WithBase
import qualified WithStmAndAsync

getEnvAction :: IO (Maybe Int)
getEnvAction =
    fmap
        (>>= readMaybe)
        (getEnv "ACTION")

die :: String -> IO a
die msg = putStrLn msg >> exitFailure

main :: IO ()
main = do
    action <- getEnvAction
    case action of
        Just 1 -> WithBase.main
        Just 2 -> WithStmAndAsync.main
        Just n -> die $ "Unknown ACTION: " <> show n
        Nothing -> die "Required env var: ACTION=NUMBER"