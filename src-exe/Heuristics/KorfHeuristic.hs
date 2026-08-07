module KorfHeuristic(korfHeuristic, korfIndivHeuristics) where

import LoadKorfHeuristics(lookupAll)
import Bandaged
import Data.Word(Word8)
import qualified Data.Vector.Unboxed as V

type Vector8 = V.Vector Word8
type HVector = (Word8, Vector8, Vector8, Vector8)

-- | DEFINITIVE method for estimating the minimal moves remaining at a position
korfHeuristic :: HVector -> BandagedCube -> Int
korfHeuristic heurVec (BandagedCube bc _) = (fromIntegral hDef) :: Int
    where
        (c, e1, e2) = lookupAll heurVec bc
        hDef = maximum [c, e1, e2]
        --maybe not optimal this steps, lot of aux functions and conversions [] <-> ()

--Used for debugging
korfIndivHeuristics :: HVector -> BandagedCube -> [Int]
korfIndivHeuristics hVec (BandagedCube bc _) = (map fromIntegral hs) :: [Int]
    where
        (c, e1, e2) = lookupAll hVec bc
        hs = [c, e1, e2]

