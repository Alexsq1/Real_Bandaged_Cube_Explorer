module MathematicalNotation(cornerState, edgesState, edgesSplittedState, mathToCube) where

import Cube(Cube(..), corners, edges, newCubeFromList)
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

-- | Takes lists of (cp, co) (ep, eo) and recompose a cube
mathToCube :: ([Int], [Int]) ->  -- ^ (CP, CO) or (corner permutation, corner orientation)
            ([Int], [Int])      -- ^ (EP, EO) or (edge permutation, edge orientation)
            -> Cube
mathToCube (cp, co) (ep, eo) = newCubeFromList (adjustedCorners ++ adjustedEdges ++ centers)
    where
        partialCorners = map (\x -> [3*x, 3 * x + 1, 3 * x + 2]) cp
        partialEdges = map (\x -> [2*x + 24, 2 * x + 25]) ep
        adjustedCorners = concat $ zipWith cyclePiece partialCorners co
        adjustedEdges = concat $ zipWith cyclePiece partialEdges eo
        centers = [48..53]

cyclePiece :: [Int] -> Int -> [Int]
cyclePiece x 0 = x
cyclePiece [a,b] 1 = [b,a]
cyclePiece [a,b,c] 1 = [c,a,b]
cyclePiece [a,b,c] 2 = [b,c,a]
cyclePiece x _ = x
