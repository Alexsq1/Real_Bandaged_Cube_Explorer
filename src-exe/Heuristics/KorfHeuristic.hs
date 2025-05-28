module KorfHeuristic(korfHeuristic, korfIndivHeuristics) where

import GenKorfHeuristics
import Bandaged


{-
The access to the pdb files will be made from here
-}

-- | DEFINITIVE method for estimating the minimal moves remaining at a position
korfHeuristic :: BandagedCube -> Int
korfHeuristic bc = (fromIntegral hDef) :: Int
    where
        (c, e1, e2) = lookupAll bc
        hDef = maximum [c, e1, e2]
        --maybe not optimal this steps, lot of aux functions and conversions [] <-> ()

--Used for debugging
korfIndivHeuristics :: BandagedCube -> [Int]
korfIndivHeuristics bc = (map fromIntegral hs) :: [Int]
    where
        (c, e1, e2) = lookupAll bc
        hs = [c, e1, e2]

