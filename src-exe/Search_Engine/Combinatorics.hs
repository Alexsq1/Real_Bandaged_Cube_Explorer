module Combinatorics(factorialNumbering, nprNumbering) where

import Data.List
import Data.Maybe

-- | Recieves a permutation of elements and returns its numeration

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

factorial :: Int -> Int
factorial n = product [2 .. ( n)]

-- | Recieves a variation of elements and returns its numeration

nprNumbering :: [Int]       -- ^ Total elements
                -> [Int]    -- ^ The variation
                -> Int
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


--variationsCardinal :: Int -> Int -> Int
--variationsCardinal n r = product [(n - r + 1 ) .. n]

--variations :: Int -> [Int] -> [[Int]]
--variations 0 _ = [[]]
--variations _ [] = [[]]
--variations k xs = [y:ys | (y,rest) <- select xs, ys <- variations (k-1) rest]
--    where
--        select [] = []
--        select (x:xs) = (x,xs) : [(y,x:ys) | (y,ys) <- select xs]
