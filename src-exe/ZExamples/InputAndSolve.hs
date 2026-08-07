module InputAndSolve(inputAndSolve) where

import Data.Maybe(fromMaybe, isJust)

--import Visualizator
import InputCube(bandagedCubeScratchIO)

import Moves(Algorithm(..))

import SolvingStrategies
import LoadKorfHeuristics(loadVectors)
import Search(extractAlg, SolutionInfo(..))
import Data.Word(Word8)

inputAndSolve :: Word8 -> IO ()
inputAndSolve n = do

    --(bc, scheme) <- bandagedCubeScratchIO
    (bc, _) <- bandagedCubeScratchIO

    --manimRecomendedVisualizer (stdCube bc) scheme (Algorithm [])
    heurVectors <- loadVectors n

    let possibleSolution = (smartKorfSolver heurVectors bc)
    let solutionSS = fromMaybe (SolutionInfo { solutionSI = Algorithm [],
                                 lAlg = 0, sizeTree = 0, visited = 0, branchingFactor = 0,
                                  exploredRatio = 0, prunedRatio = 0, exploredDepthsSI  = []})
                                  possibleSolution
    --let solutionSS = smartKorfSolver heurVectors bc

    putStrLn ("Exists solution? " ++ show (isJust possibleSolution))
    putStrLn ("Solution algorithm: " ++ show ((extractAlg)  solutionSS))
    putStrLn ("\nSolution data: \n" ++ (show solutionSS))

--    manimRecomendedVisualizer (stdCube bc) scheme (fromJust solution)
