module Main where

import InputAndSolve
import StoreKorfHeuristics(genPDBs)

main :: IO ()
main = do
    InputAndSolve.inputAndSolve 7
    --genPDBs 7
