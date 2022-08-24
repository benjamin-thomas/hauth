module ExceptionHandling where

import Control.Exception (Exception, catch, throw)
import Text (toList)
import Prelude (Either (Left, Right), IO, error, print, putStrLn, ($), (<>))

{-
Three types of exceptions exist:
    - impure exceptions: exception thrown inside a pure context!
    - synchronous exceptions: exception generated by the current thread
    - asynchronous exceptions: exception generated by another thread or the runtime system
-}

-- Impure exception
isBelow10 :: Int -> Bool
isBelow10 n = (n < 10) || error "above 10!"

{-
ghci> isBelow10 5
True

ghci> isBelow10 11
*** Exception: above 10!
CallStack (from HasCallStack):
  error, called at src/ExceptionHandling.hs:14:40 in hauth-0.1.0.0-inplace:ExceptionHandling
-}

isBelow11 :: Int -> Either Text ()
isBelow11 n = if n < 11 then Right () else Left (error "above 11!")

result :: Either Text ()
result = isBelow11 20 -- exception won't be thrown here, lazy!

run :: IO ()
run = case result of
    Left e -> do
        -- exception will be thrown here, upon execution
        putStrLn "Something's wrong!"
        putStrLn (e |> toList)
    Right _ ->
        putStrLn "All good!"

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

doFail :: IO ()
doFail =
    throw ServerOnFireException `catch` (\e -> print (e :: ServerException))

run2 :: IO () -> IO ()
run2 action =
    action
        -- `catchAny` (\e -> print (e :: MyException)) -- can't find a way to import catchAny, page 31
        `catch` (\e -> print (e :: ServerException))
        `catch` (\e -> print (e :: MyException))

doFail2 :: IO ()
doFail2 =
    run2 (throw ServerOnFireException)

{-
I don't understand why this function can't be defined.
I get this compile error:
    => Data constructor not in scope: MyException
But MyException has the same shape as ServerException (???)

doFail3 :: IO ()
doFail3 =
    run2 (throw MyException)
-}
