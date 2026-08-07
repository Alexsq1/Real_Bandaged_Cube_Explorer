module IndexHeuristics(cornersKey, edgesKey, edgesKeyFst, edgesKeySnd, keysToCube) where

import Cube(Cube(..))
import Combinatorics(factorialEncode, nprEncode, factorialDecode, nprDecode, encodeCO, encodeEO, decodeCO, decodeEO)
import MathematicalNotation(cornerState, edgesState, mathToCube)
import Data.List(sortBy)

--Max. Int: 2 ^63 -1 = 9.223.372.036.854.775.807. Keys are in range

-- | Returns the key of the corners of a BCube (in range [0, 88179839])
cornersKey :: Cube -> Int
cornersKey bc = (permKey * 3 ^ (7 :: Int)) + orKey
    where
        (perm, ori) = cornerState bc
        permKey = factorialEncode perm
        orKey = encodeCO ori

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

-- | Recompose a cube from values of keys of 3 pieces
keysToCube :: (Int, Int, Int) -> Cube
keysToCube (ck, e1k, e2k) = mathToCube (cp, co) (ep, eo)
    where
        --Separate permutation and orientation of each piece type
       (cpk, cok) = quotRem ck (3 ^ (7 :: Int)) 
       (e1pk, e1ok) = quotRem e1k (2 ^ (6 :: Int)) 
       (e2pk, e2ok) = quotRem e2k (2 ^ (6 :: Int)) 
       --Decode perm and orientation, join edges
       cp = factorialDecode cpk
       co = decodeCO cok
       ep = nprDecode e1pk ++ nprDecode e2pk
       eo = decodeEO e1ok ++ decodeEO e2ok

