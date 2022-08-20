module Main where

import Prelude (IO, putStrLn)

import qualified MyLib (hello)

main :: IO ()
main = do
  putStrLn "Hello, from main!"
  MyLib.hello
