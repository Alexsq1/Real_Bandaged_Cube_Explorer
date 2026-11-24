module Main where

import InputAndSolve
import StoreKorfHeuristics
--import OneSolve
--import HeuristicsProfile

main :: IO ()
main = do
    putStrLn "Wellcome to Real Bandaged Cube Explorer!"

    let d = 7
    genPDBsIfMissing d
    InputAndSolve.inputAndSolve d
