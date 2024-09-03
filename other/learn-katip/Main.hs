module Main (main) where

import Control.Exception
import Data.Text (Text)
import Katip
import qualified Lib
import System.IO (stdout)

{-

Terminal 1:
  ./manage/cabal_repl learn-katip

Terminal 2:
  ./manage/reload_repl_on_change :main

 -}

withLogEnv :: (LogEnv -> IO a) -> IO a
withLogEnv = bracket createLogEnv closeScribes
  where
    createLogEnv = do
        logEnv <- initLogEnv "learn-katip" "dev"
        stdoutScribe <- mkHandleScribe ColorIfTerminal stdout (permitItem DebugS) V2
        registerScribe "stdout" stdoutScribe defaultScribeSettings logEnv

logSomething :: (KatipContext m) => m ()
logSomething = do
    $(logTM) InfoS "Log in no namespace"
    katipAddNamespace "ns1" $
        $(logTM) InfoS "Log in ns1"
    katipAddNamespace "ns2" $ do
        $(logTM) WarningS "Log in ns2"
        katipAddNamespace "ns3" $
            katipAddContext (sl "userId" (123 :: Int) <> sl "userName" ("Bob" :: Text)) $ do
                $(logTM) InfoS "Log ns2.ns3 (!) with userId context"
                $(logTM) ErrorS "Something bad happened!"
                $(logTM) CriticalS "OMG!!"
    katipAddContext (sl "playing" True) $ do
        $(logTM) DebugS "This is a debug message"

runKatip :: IO ()
runKatip = withLogEnv $ \logEnv -> do
    -- The empty tuple corresponds to an initial empty "payload"
    -- mempty corresponds to an empty "namespace"
    runKatipContextT logEnv () mempty logSomething
    runKatipContextT logEnv () mempty Lib.logSomeMore

main :: IO ()
main = do
    putStrLn "Learning Katip3"
    putStrLn $ "learn: " <> show Lib.hello
    runKatip