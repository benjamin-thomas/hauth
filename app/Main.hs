module Main where

import qualified MyLib (hello)
import Relude (IO, Text, putStrLn)

main :: IO ()
main = do
  putStrLn "Hello, from main!"
  putStrLn MyLib.hello
