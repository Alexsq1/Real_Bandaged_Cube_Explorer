module InputAndSolve(inputAndSolve) where

import Data.Maybe(fromJust)

import Bandaged

import Visualizator
import InputCube
--import Moves(Algorithm(..))

--import Heuristic
import SolvingStrategies

inputAndSolve :: IO ()
inputAndSolve = do

    bc <- bandagedCubeScratchIO
    --manimRecomendedVisualizer (stdCube bc) (Algorithm [])

    let solution2 = smartKorfSolver bc
    putStrLn ("\n\nSolution found: " ++ (show solution2) ++ "\n\n")

    manimRecomendedVisualizer (stdCube bc) (fromJust solution2)
