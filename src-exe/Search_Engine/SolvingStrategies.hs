--module SolvingStrategies(iddfsSolver, kociembaSolver, smartKorfSolver) where
module SolvingStrategies(smartKorfSolver) where


import Bandaged(BandagedCube(..), solvedBC)
import Solvability(isSolvable)

import Search
import KorfHeuristic
import MoveGeneration(notBlockedMoves)

import Data.Word(Word8)
import qualified Data.Vector.Unboxed as V

type Vector8 = V.Vector Word8
type HVector = (Vector8, Vector8, Vector8)
 
-- -- | Solves the cube with iddfs algorithm (deprecated in the future)
-- iddfsSolver :: BandagedCube -> Maybe SearchingState
-- iddfsSolver bc = genericSearch bc (solvedBC) sixAxis (const 0)
-- 
-- kociembaState :: BandagedCube -> Bool
-- kociembaState bc = (sumOrientations == 0) && (sort middleEdges == [4 .. 7])
--     where
--         (_, co) = cornerState bc
--         (ep, eo) = edgesState bc
--         middleEdges = ((take 4) . (drop 4)) ep
--         sumOrientations = sum co + sum eo
-- 
-- -- | Solves the cube with the Kociemba Algorithm. Might not end depending on the bandages
-- kociembaSolver :: BandagedCube -> Maybe SearchingState
-- kociembaSolver bc
--     | (isJust algStep1) && (isJust algStep2) = Just ((fromJust algStep1) <> (fromJust algStep2))
--     | otherwise = Nothing
--     where
--         algStep1 = genericSearch bc (kociembaState) sixAxis (const 0)
--         bcIntermediate = algStep1 >>= (\algUnpack -> tryToExecuteAlg bc algUnpack)
--         --algStep2 = bcIntermediate >>= (\bcUnpack -> genericSearch bcUnpack solvedBC kociembaMoves (korfHeuristic hv))
--         algStep2 = bcIntermediate >>= (\bcUnpack -> genericSearch bcUnpack solvedBC kociembaMoves (const 0))
--         --needs refactor

-- | Solves the cube optimally with the Korf algorithm. Use only the movable faces
smartKorfSolver :: HVector -> BandagedCube -> Maybe SolutionInfo
smartKorfSolver hVec bc 
    | isSolvable bc = genericSearch bc (solvedBC) (notBlockedMoves bc) (korfHeuristic hVec)
    | otherwise = Nothing
--Here call to checking if solvable

--adjust korfHeuristic

-- | Allows the Korf algorithm to specify the layers used to generate moves
--korfLayersSolver :: [Face] -> BandagedCube -> Maybe Algorithm
--korfLayersSolver fs bc = genericSearch bc (solvedBC) (freeFaces fs) korfHeuristic