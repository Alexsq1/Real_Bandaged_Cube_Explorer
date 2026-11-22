module InputAndSolve(inputAndSolve) where

import Data.Maybe(isJust, fromJust)

import Visualizator
import InputCube(bandagedCubeScratchIO)

import Bandaged(BandagedCube(..))
import Moves(Algorithm(..))

import SolvingStrategies
import LoadKorfHeuristics(loadVectors)
import Search(extractAlg)
import Data.Word(Word8)

inputAndSolve :: Word8 -> IO ()
inputAndSolve n = do

    (bc, scheme) <- bandagedCubeScratchIO
    --(bc, _) <- bandagedCubeScratchIO

    --manimRecomendedVisualizer (stdCube bc) scheme (Algorithm [])

    heurVectors <- loadVectors n
    let possibleSolution = (smartKorfSolver heurVectors bc)

    putStrLn "Searching for a solution..."

    let alg = if (isJust possibleSolution) 
                then (extractAlg (fromJust possibleSolution))
                else (Algorithm []) 

    if (isJust possibleSolution) 
        then (putStrLn ("\n" ++ (show (fromJust possibleSolution))))
        else (putStrLn "The cube is unsolvable. Please, check the input:\n")

    manimRecomendedVisualizer (stdCube bc) scheme alg
