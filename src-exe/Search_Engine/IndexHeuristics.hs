module IndexHeuristics(cornersKey, edgesKey, edgesKeyFst, edgesKeySnd) where

import Bandaged
import Combinatorics
import MathematicalNotation(cornerState, edgesState)
import Data.List(sortBy)

-- | Returns the key of the corners of a BCube
cornersKey :: BandagedCube -> Int
cornersKey bc = (permKey * 3 ^ (7 :: Int)) + orKey
    where
        (perm, ori) = cornerState bc
        permKey = factorialNumbering perm
        orKey = sum [(3 ^ i * ori !! (6 - i)) | i <- [0 .. 6]]

-- | Returns the key of the first 6 edges
edgesKeyFst :: BandagedCube -> Int
edgesKeyFst c = fst (edgesKey c)

-- | Returns the key of the second 6 edges
edgesKeySnd :: BandagedCube -> Int
edgesKeySnd c = snd (edgesKey c)

-- | Returns the key of the halves of the edges of BCube
edgesKey :: BandagedCube -> (Int, Int)
edgesKey bc = (keyFst, keySnd)
    where
        (perm, ori) = edgesState bc
        allp = zip3 [0..11] perm ori

        sortedEdges = sortBy (\(_, p1, _) (_, p2, _) -> compare p1 p2) allp

        (iReorder, _, oReorder) = unzip3 sortedEdges

        keyFst = indexHalfE (take 6 iReorder) (take 6 oReorder)
        keySnd = indexHalfE (drop 6 iReorder) (drop 6 oReorder)

        --keyFst = indexHalfE (take 6 perm) (take 6 ori)
        --keySnd = indexHalfE (drop 6 perm) (drop 6 ori)

indexHalfE :: [Int] -> [Int] -> Int
indexHalfE perm0 ori0 = (permKey * 2 ^ (6 :: Int)) + orKey
    where
        permKey = nprNumbering [0..11] perm0
        orKey = sum [(2 ^ i * ori0 !! (5 - i)) | i <- [0 .. 5]]


