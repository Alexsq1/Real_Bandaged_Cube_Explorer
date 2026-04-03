module IndexHeuristics(cornersKey, edgesKey, edgesKeyFst, edgesKeySnd) where

import Cube(Cube(..))
import Combinatorics(nprEncode, factorialEncode, encodeCO, encodeEO)
import MathematicalNotation(cornerState, edgesState)
import Data.List(sortBy)

-- | Returns the key of the corners of a BCube (in range [0, 88179838])
cornersKey :: Cube -> Int
cornersKey bc = (permKey * 3 ^ (7 :: Int)) + orKey
    where
        (perm, ori) = cornerState bc
        permKey = factorialEncode perm
        orKey = encodeCO ori
        --orKey = baseToNum (base 3) (init ori)
        --orKey = baseToNum [729,243,81,27,9,3,1] (init ori)

-- | Returns the key of the first 6 edges (in range [0,42577919])
edgesKeyFst :: Cube -> Int
edgesKeyFst c = fst (edgesKey c)

-- | Returns the key of the second 6 edges
edgesKeySnd :: Cube -> Int
edgesKeySnd c = snd (edgesKey c)

-- | Returns the key of the halves of the edges of BCube
edgesKey :: Cube -> (Int, Int)
edgesKey bc = (keyFst, keySnd)
    where
        (perm, ori) = edgesState bc
        allp = zip3 [0..11] perm ori

        sortedEdges = sortBy (\(_, p1, _) (_, p2, _) -> compare p1 p2) allp

        (iReorder, _, oReorder) = unzip3 sortedEdges

        keyFst = indexHalfE (take 6 iReorder) (take 6 oReorder)
        keySnd = indexHalfE (drop 6 iReorder) (drop 6 oReorder)

indexHalfE :: [Int] -> [Int] -> Int
indexHalfE perm0 ori0 = (permKey * 2 ^ (6 :: Int)) + orKey
    where
        permKey = nprEncode perm0
        orKey = encodeEO ori0
        --orKey = baseToNum (base 2) o
        --orKey = baseToNum [32,16,8,4,2,1] ori0


--Refactor: fucntions for perm, functions for or, functions for mix
--Needing a function that takes 3 keys and recompose a cube (for non-used pieces, take 0)