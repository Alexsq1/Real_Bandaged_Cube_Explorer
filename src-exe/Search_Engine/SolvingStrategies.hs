module SolvingStrategies(iddfsSolver, kociembaSolver, smartKorfSolver) where


import Bandaged(BandagedCube(..), solvedBC, tryToExecuteAlg)
import Moves

import Search
import KorfHeuristic
import MathematicalNotation(edgesState, cornerState)

import MoveGeneration(sixAxis, kociembaMoves, notBlockedMoves)

import Data.Maybe(fromJust, isJust)
import Data.List(sort)

-- | Solves the cube with iddfs algorithm (deprecated in the future)
iddfsSolver :: BandagedCube -> Maybe Algorithm
iddfsSolver bc = genericSearch bc (solvedBC) sixAxis (const 0)

kociembaState :: BandagedCube -> Bool
kociembaState bc = (sumOrientations == 0) && (sort middleEdges == [4 .. 7])
    where
        (_, co) = cornerState bc
        (ep, eo) = edgesState bc
        middleEdges = ((take 4) . (drop 4)) ep
        sumOrientations = sum co + sum eo

-- | Solves the cube with the Kociemba Algorithm. Might not end depending on the bandages
kociembaSolver :: BandagedCube -> Maybe Algorithm
kociembaSolver bc
    | (isJust algStep1) && (isJust algStep2) = Just ((fromJust algStep1) <> (fromJust algStep2))
    | otherwise = Nothing
    where
        algStep1 = genericSearch bc (kociembaState) sixAxis (const 0)
        bcIntermediate = algStep1 >>= (\algUnpack -> tryToExecuteAlg bc algUnpack)
        algStep2 = bcIntermediate >>= (\bcUnpack -> genericSearch bcUnpack solvedBC kociembaMoves korfHeuristic)

-- | Solves the cubo optimally with the Korf algorithm. Use only the movable faces
smartKorfSolver :: BandagedCube -> Maybe Algorithm
smartKorfSolver bc = genericSearch bc (solvedBC) (notBlockedMoves bc) korfHeuristic

-- | Allows the Korf algorithm to specify the layers used to generate moves
--korfLayersSolver :: [Face] -> BandagedCube -> Maybe Algorithm
--korfLayersSolver fs bc = genericSearch bc (solvedBC) (freeFaces fs) korfHeuristic