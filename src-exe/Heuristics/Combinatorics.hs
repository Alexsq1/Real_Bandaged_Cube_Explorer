module Combinatorics(factorialEncode, factorialDecode, nprEncode, nprDecode,
                    encodeCO, decodeCO, encodeEO, decodeEO) where

import Data.List(elemIndex, delete, (\\), insert)
import Data.Maybe(fromJust)

{- GENERALS -}

-- | Takes base elements (can't be lazy) and the numbers (should be of equal lengths). Returns the number represented in that base. 
baseToNum :: [Int]                  -- ^ Base multiplicators in decreasing order
            -> [Int]                -- ^ Elements (each must be < than its multiplicator to be injective)
            -> Int
baseToNum b els = sum $ zipWith (*) b els

-- | Recieves base elements and number, writes the number in that base
numToBase :: [Int]                  -- ^ Base multiplicators in decreasing order (can't be lazy)
            -> Int                  -- ^ Number
            -> [Int]
numToBase [] _ = []
numToBase (x:xs) i = q : numToBase xs r
    where
        (q, r) = quotRem i x
        --tail recursion optim?

-- | Takes the first ocurrence of elem in a list and removes it
firstOcurrence :: Int -> [Int] -> (Int, [Int])
firstOcurrence n xs = (i, xs2)
    where 
        i = fromJust (elemIndex n xs)
        xs2 = delete n xs

{- PERMUTATION ENCODE AND DECODE -}

-- | Assign the number of a permutation (in n = 8). Input must be a permutation of [0..7]
factorialEncode :: [Int] -> Int
factorialEncode xs = ((baseToNum fList) . (perm2Lehmer [0 .. 7] )) xs
    where
        fList = [5040,720,120,24,6,2,1,1]

-- | Assign the number of a variation (in n=12, r=6). Input must be 6 numbers of [0..11] without repetitions
nprEncode :: [Int] -> Int
nprEncode xs = ((baseToNum fList) . (perm2Lehmer [0 .. 11])) xs
    where
        fList = [55440,5040,504,56,7,1]

-- | Assign the permutation of a number (in n = 8). Input must be in range [0, 40319]
factorialDecode :: Int          -- ^ Number of elements
                -> [Int]
factorialDecode = ((lehmer2Perm [0 .. 7] ) . (numToBase fList))
    where
        fList = [5040,720,120,24,6,2,1,1]

-- | Returns the permutation of a number (in n = 12, r = 6). Input must be in range [0, 665279]
nprDecode :: Int -> [Int]
nprDecode = ((lehmer2Perm [0 .. 11]). (numToBase fList))
    where
        fList = [55440,5040,504,56,7,1]

-- | Takes the original elements sorted and a permutation. Returns the Lehmer code
perm2Lehmer :: [Int]                -- ^ All possible elements sorted
                -> [Int]            -- ^ Permutation
                -> [Int]            -- ^ Lehmer code
perm2Lehmer [] _ = []
perm2Lehmer _ [] = []
perm2Lehmer orig (x:xs) = th : perm2Lehmer xs2 xs
    where
        (th, xs2) = firstOcurrence x orig

-- | Takes total elements sorted and a Lehmer code and returns the permutation 
lehmer2Perm :: [Int] -> [Int] -> [Int]
lehmer2Perm total t = l2P [] total t
    where
        l2P _ _ [] = []
        l2P acc tot (r:rs) = i : l2P (insert i acc) tot rs
            where i = (tot \\ acc) !! r

{- ORIENTATION -}

-- | Recieves list of 8 numbers of [0,1,2]. Returns number in base 3. Should sum multiple of 3 (last element is not considered)
encodeCO :: [Int] -> Int
encodeCO xs = baseToNum [729,243,81,27,9,3,1] (init xs)

-- | Recieves number of corner key (in the range [0, 2186]) and returns the co. Calculates the last element such that sum is multiple of 3
decodeCO :: Int -> [Int]
decodeCO n = first7 ++ [l8]
    where
        first7 = numToBase [729,243,81,27,9,3,1] n
        l8 = (3 - sum first7) `mod` 3

-- | Recieves list of 6 numbers of [0,1]. Returns number in base 2
encodeEO :: [Int] -> Int
encodeEO = baseToNum [32,16,8,4,2,1]

-- | Recieves number of edge key (in the range [0, 63]) and returns the eo.
decodeEO :: Int -> [Int]
decodeEO = numToBase [32,16,8,4,2,1]

{- MEMOIZED RESULTS:
take 8 factorials: [5040,720,120,24,6,2,1,1]
take 6 npr 12 6: [55440,5040,504,56,7,1]
take 8 (base 3): [2187,729,243,81,27,9,3,1] --careful of not using the first
take 6 (base 2): [32,16,8,4,2,1]
 -}