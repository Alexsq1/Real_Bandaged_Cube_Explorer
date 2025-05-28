module InputAndSolve(inputAndSolve) where

import Data.Maybe(fromJust)

import Bandaged

import Visualizator
import InputCube

--import Moves(Algorithm(..))

--import KorfHeuristic
import SolvingStrategies

inputAndSolve :: IO ()
inputAndSolve = do

    (bc, scheme) <- bandagedCubeScratchIO
    --manimRecomendedVisualizer (stdCube bc) scheme (Algorithm [])

    let solution2 = smartKorfSolver bc
    putStrLn ("\n\nSolution found: " ++ (show $ fromJust solution2) ++ "\n\n")

    manimRecomendedVisualizer (stdCube bc) scheme (fromJust solution2)
