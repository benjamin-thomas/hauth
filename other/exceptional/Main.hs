{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid restricted function" #-}

module Main where

{-

Chapter 2 : Exception Handling
==============================

cabal run exceptional
 -}

isBelow10 :: Int -> Either String ()
isBelow10 x = if x < 10 then Right () else Left (error "above 10!")

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

main :: IO ()
main = do
    check1
    check2
    putStrLn "Finished!"