module MathematicalNotation(cornerState, edgesState) where

import Cube
import Bandaged

-- | Returns the state of corners in "mathematical" notation
cornerState :: BandagedCube -> ([Int], [Int])
cornerState (BandagedCube cube _) = (perm, ori)
    where
        xs = zip [0 .. 23] (corners cube)
        xs2 = filter (\t -> (snd t) `mod` 3 == 0 ) xs
        perm = map (\(_,x) -> x `div` 3) xs2
        ori = map (\(x,_) -> x `mod` 3) xs2


-- | Returns the state of edges in "mathematical" notation
edgesState :: BandagedCube -> ([Int], [Int])
edgesState (BandagedCube cube _) = (perm, ori)
    where
        xs = zip [24 .. 47] (edges cube)        
        xs2 = filter (\(_, y) -> (y `mod` 2 == 0)) xs
        perm = map (\(_,x) -> (x - 24) `div` 2) xs2
        ori = map (\(x,_) -> x `mod` 2) xs2
