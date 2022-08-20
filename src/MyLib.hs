module MyLib (hello) where

import Prelude (IO, putStrLn)

hello :: IO ()
hello = putStrLn "Hello, from lib!"
