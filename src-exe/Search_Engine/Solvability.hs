module Solvability(isSolvable, numSwaps) where

import Bandaged
import MathematicalNotation
import Data.List(sort)

isSolvable :: BandagedCube -> Bool
isSolvable b = fdtalTheorem b

{- 
Good input: no repeated pieces, 54 stickers
Solvable 3x3
Non movable blocks solved
Joint pieces adyacent in solved state (solvable)
 -}

--Moved to InputCube, restore when deprecate it
--goodInput :: BandagedCube -> Bool
--goodInput (BandagedCube bc blocks) = (sort . allPieces) bc == [0..53]
--                            && all (\b -> minimum b >= 0 && maximum b < 54) blocks


fdtalTheorem :: BandagedCube -> Bool
fdtalTheorem b = ((sum co) `mod` 3 == 0) && 
              ((sum eo) `mod` 2 == 0) &&
              ((numSwaps cp) `mod` 2) == ((numSwaps ep) `mod` 2)
    where
        (cp, co) = cornerState b
        (ep, eo) = edgesState b

numSwaps :: [Int] -> Int
numSwaps xs 
    | sort xs == [0..maximum xs] =  ns (length xs -1) xs
    | otherwise = error "numSwaps does only works with permutations of consecutive elements starting at 0 without repetitions"
    where
        ns :: Int -> [Int] -> Int
        ns (-1) _ = 0
        ns i ys
            | (ys !! i) == i = ns (i-1) ys
            | otherwise = 1 + ns i (swap i (ys !! i) ys)

swap :: Int -> Int -> [a] -> [a]
swap a b xs = s a b (xs !! a) (xs !! b) (0) xs
    where
        s _ _ _ _ _ [] = []
        s aa bb fa fb i (y:ys)
            | aa == i = fb : s aa bb fa fb (i+1) ys
            | bb == i = fa : s aa bb fa fb (i+1) ys
            | otherwise = y : s aa bb fa fb (i+1) ys