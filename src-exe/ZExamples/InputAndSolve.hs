module InputAndSolve(inputAndSolve) where

import Data.Maybe(fromJust, fromMaybe)

import Bandaged

import Visualizator
import InputCube

import Moves(Algorithm(..))

--import KorfHeuristic
import SolvingStrategies

inputAndSolve :: IO ()
inputAndSolve = do

    (bc, scheme) <- bandagedCubeScratchIO
    --manimRecomendedVisualizer (stdCube bc) scheme (Algorithm [])

    let solution = smartKorfSolver bc
    let (Algorithm moves) = fromMaybe (Algorithm[]) solution
    putStrLn ("\n\nSolution found: " ++ (show $ fromJust solution) ++ 
                "\n" ++ (show (length moves)) ++ " moves" ++  "\n\n")


    manimRecomendedVisualizer (stdCube bc) scheme (fromJust solution)
