module Main where

import qualified MyLib

{-

cabal run hauth

 -}
main :: IO ()
main = do
  putStrLn "Hello, from main!"
  MyLib.start
