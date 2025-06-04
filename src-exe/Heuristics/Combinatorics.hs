module Combinatorics(factorialNumbering, nprNumbering) where

import Data.List(elemIndex, sort, delete)
import Data.Maybe(fromJust)

-- | Recieves a permutation of elements and returns its numeration
factorialNumbering :: [Int] -> Int
factorialNumbering xp = factorialNumberingGlobal xp (sort xp)
    where
        factorialNumberingGlobal :: [Int] -> [Int] -> Int
        factorialNumberingGlobal [] _ = 0
        factorialNumberingGlobal (x:xs) orig = thisElem + factorialNumberingGlobal xs xs2
            where
                (i, xs2) = firstOcurrence x orig
                thisElem = i * factorial (length xs)
--If it has efficiency issues, it can be interested to compute factorials in the opposite order.

factorial :: Int -> Int
factorial n = product [2 .. n]

-- | Recieves a variation of elements and returns its numeration
nprNumbering :: [Int]       -- ^ Total elements
                -> [Int]    -- ^ The variation
                -> Int
nprNumbering totalNumbers perm = npr perm totalNumbers 0 n (n - r)
    where
        n = length totalNumbers
        r = length perm

        npr :: [Int] -> [Int] -> Int -> Int -> Int -> Int
        npr [] _ _ _ _ = 0
        npr (x:xs) totalElems index cardTotalElems denom = thisElem + npr xs xs2 (index + 1) cardTotalElems denom
            where
                (i, xs2) = firstOcurrence x totalElems
                thisElem = i * (product [denom + 1 .. cardTotalElems - 1 - index])

firstOcurrence :: Int -> [Int] -> (Int, [Int])
firstOcurrence n xs = (i, xs2)
    where 
        i = fromJust (elemIndex n xs)
        xs2 = delete n xs
