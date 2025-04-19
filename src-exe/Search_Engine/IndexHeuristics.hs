module IndexHeuristics(cornersKey, edgesKey, edgesKeyFst, edgesKeySnd, cornerState, edgesState) where

import Bandaged
import Cube
import Data.List
import Data.Maybe

noHeuristic :: BandagedCube -> Int
noHeuristic bc
    | solvedBC bc = 0
    | otherwise = 1


-- | Returns the key of the corners of a BCube
cornersKey :: BandagedCube -> Int
cornersKey bc = (permKey * 3 ^ (7 :: Int)) + orKey
    where
        (perm, ori) = cornerState bc
        permKey = factorialNumbering perm
        orKey = sum [(3 ^ i * ori !! (6 - i)) | i <- [0 .. 6]]

--        xs = zip [0 .. 23] (corners cube)
--        xs2 = filter (\t -> (snd t) `mod` 3 == 0 ) xs
--        perm = map (\(_,x) -> x `div` 3) xs2
--        ori = map (\(x,_) -> x `mod` 3) xs2

--True: first 6, false, following 6

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
        keyFst = indexHalf (take 6 perm) (take 6 ori)
        keySnd = indexHalf (drop 6 perm) (drop 6 ori)

indexHalf :: [Int] -> [Int] -> Int
indexHalf perm0 ori0 = (permKey * 2 ^ (6 :: Int)) + orKey
    where
        permKey = nprNumbering [0..11] perm0
        orKey = sum [(2 ^ i * ori0 !! (5 - i)) | i <- [0 .. 5]]



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

factorial :: Int -> Int
factorial n = product [2 .. ( n)]

--variationsCardinal :: Int -> Int -> Int
--variationsCardinal n r = product [(n - r + 1 ) .. n]

--variations :: Int -> [Int] -> [[Int]]
--variations 0 _ = [[]]
--variations _ [] = [[]]
--variations k xs = [y:ys | (y,rest) <- select xs, ys <- variations (k-1) rest]
--    where
--        select [] = []
--        select (x:xs) = (x,xs) : [(y,x:ys) | (y,ys) <- select xs]

factorialNumbering :: [Int] -> Int
factorialNumbering xp = factorialNumberingGlobal xp (sort xp)
    where
        factorialNumberingGlobal :: [Int] -> [Int] -> Int
        factorialNumberingGlobal [] _ = 0
        factorialNumberingGlobal (x:xs) orig = thisElem + factorialNumberingGlobal xs xs2
            where
                (i, xs2) = firstOcurrence x orig
                l = length xs
                thisElem = ( i) * factorial l

nprNumbering :: [Int] -> [Int] -> Int
nprNumbering totalNumbers perm = npr perm totalNumbers 0 m (m - r)
    where
        m = length totalNumbers
        r = length perm

        npr :: [Int] -> [Int] -> Int -> Int -> Int -> Int
        npr [] _ _ _ _ = 0
        npr (x:xs) total index n denom = thisElem + npr xs xs2 (index + 1) n denom
            where
                (i, xs2) = firstOcurrence x total
                thisElem = i * (product [denom + 1 .. n - 1 - index])

firstOcurrence :: Int -> [Int] -> (Int, [Int])
firstOcurrence n xs = (i, xs2)
    where 
        i = fromJust (elemIndex n xs)
        xs2 = delete n xs
