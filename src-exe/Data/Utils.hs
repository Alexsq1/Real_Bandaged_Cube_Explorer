module Utils where

import Data.Function
import Data.List(sortBy)

adjSublist :: [a] -> Int -> Int -> [a]
adjSublist xs mini maxi = (take (1 + maxi - mini) (drop mini xs) )

remove :: Eq a => [a] -> a -> [a]
remove xs this_elem = filter (/= this_elem) xs

isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation [] [] = True
isPermutation (x:xs) ys = 
                (length (x:xs) == length ys) 
                && isPermutation (remove xs x) (remove ys x)
isPermutation _ _ = False

uniqueElems :: Eq a => [a] -> Bool
uniqueElems [] = True
uniqueElems (x:xs) = (not (elem x xs)) && uniqueElems xs

sort_by_snd :: Ord b => [(a,b)] -> [(a,b)]
sort_by_snd xs = sortBy (compare `on` snd) xs

inRange :: Int -> Int -> Int -> Bool
inRange x mini maxi = x <= maxi && x >= mini

swap :: Int -> Int -> [a] -> [a]
swap a b xs
    | a == b = xs
    | a < b = (adjSublist xs 0 (a-1)) ++ [xs !! b] ++ (adjSublist xs (a+1) (b-1)) ++ [xs !! a] ++ (adjSublist xs (b+1) (length xs -1))
    | otherwise = (adjSublist xs 0 (b-1)) ++ [xs !! a] ++ (adjSublist xs (b+1) (a-1)) ++ [xs !! b] ++ (adjSublist xs (a+1) (length xs -1))

removeDups :: Eq a => [a] -> [a]
removeDups [] = []
removeDups (x:xs) = x : removeDups (xs2)
    where xs2 = remove xs x

factorial :: Integer -> Integer
factorial n = product [x | x <- [2 .. n]]

