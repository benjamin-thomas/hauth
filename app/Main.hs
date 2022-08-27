module Main where

import qualified MyLib (hello)

main :: IO ()
main = do
  putStrLn "Hello, from main!"
  putStrLn MyLib.hello
