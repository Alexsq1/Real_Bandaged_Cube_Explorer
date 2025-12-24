module KorfHeuristic(korfHeuristic, korfIndivHeuristics) where

import GenKorfHeuristics
import LoadKorfHeuristics
import Bandaged
import Data.Word(Word8)
import qualified Data.Vector.Unboxed as V


-- | DEFINITIVE method for estimating the minimal moves remaining at a position
korfHeuristic :: HVector -> BandagedCube -> Int
korfHeuristic heurVec bc = (fromIntegral hDef) :: Int
    where
        (c, e1, e2) = lookupAll heurVec bc
        hDef = maximum [c, e1, e2]
        --maybe not optimal this steps, lot of aux functions and conversions [] <-> ()

--Used for debugging
korfIndivHeuristics :: HVector -> BandagedCube -> [Int]
korfIndivHeuristics hVec bc = (map fromIntegral hs) :: [Int]
    where
        (c, e1, e2) = lookupAll hVec bc
        hs = [c, e1, e2]

