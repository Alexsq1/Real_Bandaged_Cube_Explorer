module Main where

import InputAndSolve
import StoreKorfHeuristics
--import HeuristicsProfile


main :: IO ()
main = do
    putStrLn "Wellcome to Real Bandaged Cube Explorer!"

    let d = 8
    genPDBsIfMissing d
    InputAndSolve.inputAndSolve d
