module Main where

--import InputAndSolve
import OneSolve
--import HeuristicsProfile

main :: IO ()
main = do
    --InputAndSolve.inputAndSolve 7
    --genPDBs 7
    OneSolve.oneSolve 7

    --n <- HeuristicsProfile.prof
    --putStrLn $ show n
