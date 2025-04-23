module MoveGeneration(sixAxis, kociembaMoves, freeFaces) where

import Moves

sixAxis :: [Turn]
sixAxis = genMs (map (casualZip [1 .. 3]) [R .. ] )

kociembaMoves :: [Turn]
kociembaMoves = genMs (qt ++ ht)
    where
        qt = map (casualZip [1 .. 3]) [U, D]
        ht = map (casualZip [2]) [R, L, F, B]

freeFaces :: [Face] -> [Turn]
freeFaces xs = genMs (map (casualZip [1..3]) xs)

casualZip :: [Int] -> Face -> (Face, [Int])
casualZip xs f = (f, xs)

genMs :: [(Face, [Int])] -> [Turn]
genMs xs = concat (map indivFace xs)
    where
        indivFace :: (Face, [Int]) -> [Turn]
        indivFace = ( \(f,numsList) -> [Turn (f, n) | n <- numsList] )
