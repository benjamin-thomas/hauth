module WithBase (main) where

{-

> Modules starting with GHC. are often unstable and contain primitives.

See: https://discourse.haskell.org/t/ghc-conc-vs-stm/10264/2
 -}
import GHC.Conc (
    atomically,
    newTVarIO,
    readTVar,
    readTVarIO,
    writeTVar,
 )

asyncOperation :: Int -> IO Int
asyncOperation v = do
    tvar <- newTVarIO v
    let add1 = readTVar tvar >>= \val -> writeTVar tvar (val + 1)
    let add1Actions = replicate 100 add1
    mapM_ atomically add1Actions
    readTVarIO tvar

main :: IO ()
main = do
    putStrLn "First operation"
    n1 <- asyncOperation 0
    putStrLn ("Result 1: " <> show n1)

    putStrLn "---"

    putStrLn "Second operation"
    n2 <- asyncOperation (n1 + 1)
    putStrLn ("Result 2: " <> show n2)
