module Main where

import InputAndSolve
import StoreKorfHeuristics
--import OneSolve
--import HeuristicsProfile


-- debugging
import Cube
import Bandaged
import Moves
import MathematicalNotation
import CubeCreator
import Combinatorics
import IndexHeuristics
import CalculateKorfHeuristics

main :: IO ()
main = do
    putStrLn "Wellcome to Real Bandaged Cube Explorer!"

    let d = 3
    genPDBsIfMissing d
    --InputAndSolve.inputAndSolve d
