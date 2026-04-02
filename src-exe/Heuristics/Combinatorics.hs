module Combinatorics(factorialEncode, factorialDecode, nprEncode, nprDecode, baseToNum, base) where

import Data.List(elemIndex, delete, (\\), insert)
import Data.Maybe(fromJust)


-- | Assign the number of a permutation
factorialEncode :: [Int] -> Int
factorialEncode xs = ((baseToNum fList) . (perm2Lehmer [0 .. length xs -1])) xs
    where
        fList = take (length xs) factorials

-- | Assign the number of a variation
nprEncode :: (Int,Int) -> [Int] -> Int
nprEncode (n,r) xs = ((baseToNum fList) . (perm2Lehmer [0..n-1])) xs
    where
        fList = take (length xs) (npr n r)

-- | Returns the permutation of a number        quickCheck factInverse1
factorialDecode :: Int          -- ^ Number of elements
                -> Int          -- ^ Number 
                -> [Int]
factorialDecode n = ((lehmer2Perm [0..n-1]) . (numToBase fList))
    where
        fList = take n factorials

-- | Returns the permutation of a number
nprDecode :: (Int,Int)          -- ^ (n, r)
            -> Int              -- ^ Number 
            -> [Int]
nprDecode (n, r) = ((lehmer2Perm [0..n-1]). (numToBase fList))
    where
        fList = take r (npr n r)

-- | Takes the original elements sorted and a permutation. Returns the Lehmer code
perm2Lehmer :: [Int]                -- ^ All possible elements sorted
                -> [Int]            -- ^ Permutation
                -> [Int]            -- ^ Lehmer code
perm2Lehmer [] _ = []
perm2Lehmer _ [] = []
perm2Lehmer orig (x:xs) = th : perm2Lehmer xs2 xs
    where
        (th, xs2) = firstOcurrence x orig

-- | Takes a Lehmer code and returns the permutation 
lehmer2Perm :: [Int] -> [Int] -> [Int]
lehmer2Perm total t = l2P [] total t
    where
        l2P _ _ [] = []
        l2P acc tot (r:rs) = i : l2P (insert i acc) tot rs
            where i = (tot \\ acc) !! r

-- | Takes base elements (can be lazy) and the numbers. Returns the number represented in that base. 
baseToNum :: [Int]                  -- ^ Base multiplicators in increasing order
            -> [Int]                -- ^ Elements (each must be < than its multiplicator)
            -> Int
baseToNum b els = sum $ zipWith (*) ((reverse . (take (length els))) b) els

-- | Recieves base elements and number, writes the number in that base
numToBase :: [Int]                  -- ^ Base multiplicators in increasing order
            -> Int                  -- ^ Number
            -> [Int]
numToBase ys = ntb (reverse ys)
    where
        ntb [] _ = []
        ntb (x:xs) i = q : ntb xs r
            where
                (q, r) = quotRem i x

-- | Takes the first ocurrence of elem in a list and removes it
firstOcurrence :: Int -> [Int] -> (Int, [Int])
firstOcurrence n xs = (i, xs2)
    where 
        i = fromJust (elemIndex n xs)
        xs2 = delete n xs

-- | Lazy list of factorials (starting at 0)
factorials :: [Int]
factorials = 1 : zipWith (*) factorials [1..]

-- | Lazy list of npr
npr :: Int -> Int -> [Int]
npr n r = 1 : lzy
    where lzy = (n - r + 1) : zipWith (*) (lzy) [(n - r + 2)..]

-- | Recieves a numeric base, returns the base multipliers of it (starting at n^0)
base :: Int -> [Int]
base n = map (\x -> n ^ (x :: Int)) [0..]