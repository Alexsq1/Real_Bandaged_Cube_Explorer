--module Heuristic(noHeuristic) where
module Heuristic(noHeuristic) where

import Bandaged
--import Cube
--import Data.List
--import Data.Maybe

noHeuristic :: BandagedCube -> Int
noHeuristic bc
    | solvedBC bc = 0
    | otherwise = 1

