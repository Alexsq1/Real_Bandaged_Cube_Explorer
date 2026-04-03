module MathematicalNotation(cornerState, edgesState, edgesSplittedState) where

import Cube(Cube(..), corners, edges)
--import Bandaged(BandagedCube(..))

-- | Returns the state of corners in "mathematical" notation
cornerState :: Cube -> ([Int], [Int])
cornerState cube  = (perm, ori)
    where
        xs = zip [0 .. 23] (corners cube)
        xs2 = filter (\t -> (snd t) `mod` 3 == 0 ) xs
        perm = map (\(_,x) -> x `div` 3) xs2
        ori = map (\(x,_) -> x `mod` 3) xs2


-- | Returns the state of edges in "mathematical" notation
edgesState :: Cube -> ([Int], [Int])
edgesState cube = (perm, ori)
    where
        xs = zip [24 .. 47] (edges cube)        
        xs2 = filter (\(_, y) -> (y `mod` 2 == 0)) xs
        perm = map (\(_,x) -> (x - 24) `div` 2) xs2
        ori = map (\(x,_) -> x `mod` 2) xs2

-- | Returns the state of edges splitted in "mathematical" notation
edgesSplittedState :: Cube -> (([Int], [Int]), ([Int], [Int]))
edgesSplittedState c = (s1, s2)
    where
        (p, o) = edgesState c
        s1 = (take 6 p, take 6 o)
        s2 = (drop 6 p, drop 6 o)

recompose :: ([Int], [Int]) -> ([Int], [Int]) -> Cube
--recompose (cp, co) (ep, eo) = undefined
recompose = undefined

--it is only for heuristic generation, blocks are irrelevant
--need a cycling function for orientation (max 2, ugly hardcoded)
--must be fast. rest may be easy (*2, *3)
--don't forget centers
