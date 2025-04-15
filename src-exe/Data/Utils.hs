module Utils where

import qualified Data.Vector as V
--import Data.Int (Int8)

-- | Cycle the pieces of a vector. Input cycles list, like [[0,1,2], [3,4]]
--cyclePieces :: V.Vector Int -> [[Int]] -> V.Vector Int
--cyclePieces v cycs = (V.//) v swaps
--    where
--        swaps = concat (map (listToSwaps v) cycs)
--
--listToSwaps :: V.Vector Int -> [Int] -> [(Int, Int)]
--listToSwaps orig cycs = map (\(a, b) -> (a, orig V.! b)) t
--    where
--        t = zip cycs (last cycs : init cycs)








----------------------------------------OLD-----------------

--Might be useful:
--isPermutation :: Eq a => [a] -> [a] -> Bool
--isPermutation [] [] = True
--isPermutation (x:xs) ys = 
--                (length (x:xs) == length ys) 
--                && isPermutation (remove xs x) (remove ys x)
--isPermutation _ _ = False
--
--remove :: Eq a => [a] -> a -> [a]
--remove xs this_elem = filter (/= this_elem) xs
--
--uniqueElems :: Eq a => [a] -> Bool
--uniqueElems [] = True
--uniqueElems (x:xs) = (not (elem x xs)) && uniqueElems xs




--Might be better: validPermutation: length is 54, all between 0-54 and unique

--swap :: Int -> Int -> [a] -> [a]
--swap a b xs
--    | a == b = xs
--    | a < b = (adjSublist xs 0 (a-1)) ++ [xs !! b] ++ (adjSublist xs (a+1) (b-1)) ++ [xs !! a] ++ (adjSublist xs (b+1) (length xs -1))
--    | otherwise = (adjSublist xs 0 (b-1)) ++ [xs !! a] ++ (adjSublist xs (b+1) (a-1)) ++ [xs !! b] ++ (adjSublist xs (a+1) (length xs -1))
--adjSublist :: [a] -> Int -> Int -> [a]
--adjSublist xs mini maxi = (take (1 + maxi - mini) (drop mini xs) )
