module InputAndSolve(inputAndSolve) where

import Data.Maybe(fromJust, fromMaybe)

--import Bandaged

--import Visualizator
import InputCube

import Moves(Algorithm(..))

import SolvingStrategies
import LoadKorfHeuristics(loadVectors)

inputAndSolve :: Int -> IO ()
inputAndSolve n = do

    --(bc, scheme) <- bandagedCubeScratchIO
    (bc, _) <- bandagedCubeScratchIO
    --manimRecomendedVisualizer (stdCube bc) scheme (Algorithm [])
    heurVectors <- loadVectors n

    let solution = smartKorfSolver heurVectors bc
    let (Algorithm moves) = fromMaybe (Algorithm[]) solution
    putStrLn ("\n\nSolution found: " ++ (show $ fromJust solution) ++ 
                "\n" ++ (show (length moves)) ++ " moves" ++  "\n\n")


--    manimRecomendedVisualizer (stdCube bc) scheme (fromJust solution)
