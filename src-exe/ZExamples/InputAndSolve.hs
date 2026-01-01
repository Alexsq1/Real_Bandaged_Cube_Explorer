module InputAndSolve(inputAndSolve) where

--import Data.Maybe(fromJust, fromMaybe)
import Data.Maybe(fromJust)

--import Bandaged

--import Visualizator
import InputCube

import Moves(Algorithm(..))

import SolvingStrategies
import LoadKorfHeuristics(loadVectors)
import Search(extractAlg)

inputAndSolve :: Int -> IO ()
inputAndSolve n = do

    --(bc, scheme) <- bandagedCubeScratchIO
    (bc, _) <- bandagedCubeScratchIO

    --manimRecomendedVisualizer (stdCube bc) scheme (Algorithm [])
    heurVectors <- loadVectors n

    let solutionSS = fromJust (smartKorfSolver heurVectors bc)
    let algSolution = (extractAlg solutionSS)
    let (Algorithm moves) = algSolution

    putStrLn ("Solution algorithm: " ++ show (Algorithm moves))
    putStrLn ("\nSolution data: \n" ++ (show solutionSS))

--    manimRecomendedVisualizer (stdCube bc) scheme (fromJust solution)
