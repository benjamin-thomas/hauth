module Lib where

import Katip (
    KatipContext,
    Severity (
        AlertS,
        CriticalS,
        DebugS,
        EmergencyS,
        ErrorS,
        InfoS,
        NoticeS,
        WarningS
    ),
    katipAddContext,
    logTM,
    sl,
 )

hello :: Int
hello = 3

logSomeMore :: (KatipContext m) => m ()
logSomeMore = do
    katipAddContext (sl "playing" True) $ do
        $(logTM) DebugS "This is a debug message"
        $(logTM) InfoS "This is an info message"
        $(logTM) NoticeS "This is a notice message"
        $(logTM) WarningS "This is a warning message"
        $(logTM) ErrorS "This is an error message"
        $(logTM) CriticalS "This is a critical message"
        $(logTM) AlertS "This is an alert message"
        $(logTM) EmergencyS "This is an emergency message"