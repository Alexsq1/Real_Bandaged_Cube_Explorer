module InputAndSolve(inputAndSolve) where

import Data.Maybe(fromJust)

import Bandaged

import Visualizator
import InputCube

--import Heuristic
import SolvingStrategies

inputAndSolve :: IO ()
inputAndSolve = do

    bc <- bandagedCubeScratchIO

    let solution1 = korfSolver bc
    putStrLn ("\n\nSolution found: " ++ (show solution1) ++ "\n\n")

    manimRecomendedVisualizer (stdCube bc) (fromJust solution1)
