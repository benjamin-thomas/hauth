module Main where

import qualified MyLib (hello)
import Prelude (IO, putStrLn)

main :: IO ()
main = do
  putStrLn "Hello, from main!"

-- putStrLn MyLib.hello
