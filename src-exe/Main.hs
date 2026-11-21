module Main where

import InputAndSolve
import StoreKorfHeuristics
--import OneSolve
--import HeuristicsProfile

main :: IO ()
main = do
    putStrLn "Wellcome to Real Bandaged Cube Explorer!"

    let d = 7
    genPDBs d
    InputAndSolve.inputAndSolve d
    --OneSolve.oneSolve 7

    --n <- HeuristicsProfile.prof
    --putStrLn $ show n
