module Heuristic(noHeuristic) where

import Bandaged

noHeuristic :: BandagedCube -> Int
noHeuristic bc
    | solvedBC bc = 0
    | otherwise = 1