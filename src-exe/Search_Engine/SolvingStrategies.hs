module SolvingStrategies(iddfsSolver, kociembaState, kociembaSolver, korfSolver) where


import Bandaged(BandagedCube(..), solvedBC, tryToExecuteAlg)
--import GenKorfHeuristics
import Heuristic
import Search
import Moves
import MoveGeneration(sixAxis, kociembaMoves)
import Data.Maybe

import MathematicalNotation(edgesState, cornerState)
import Data.List(sort)

iddfsSolver :: BandagedCube -> Maybe Algorithm
iddfsSolver bc = genericSearch bc (solvedBC) sixAxis (const 0)

kociembaState :: BandagedCube -> Bool
kociembaState bc = (sumOrientations == 0) && (sort middleEdges == [4 .. 7])
    where
        (_, co) = cornerState bc
        (ep, eo) = edgesState bc
        middleEdges = ((take 4) . (drop 4)) ep
        sumOrientations = sum co + sum eo


kociembaSolver :: BandagedCube -> Maybe Algorithm
kociembaSolver bc
    | (isJust algStep1) && (isJust algStep2) = Just ((fromJust algStep1) <> (fromJust algStep2))
    | otherwise = Nothing
    where
        algStep1 = genericSearch bc (kociembaState) sixAxis (const 0)
        bcIntermediate = algStep1 >>= (\algUnpack -> tryToExecuteAlg bc algUnpack)
        algStep2 = bcIntermediate >>= (\bcUnpack -> genericSearch bcUnpack solvedBC kociembaMoves korfHeuristic)
        Algorithm xs1 = fromJust algStep1
        Algorithm xs2 = fromJust algStep2

korfSolver :: BandagedCube -> Maybe Algorithm
korfSolver bc = genericSearch bc (solvedBC) sixAxis korfHeuristic
