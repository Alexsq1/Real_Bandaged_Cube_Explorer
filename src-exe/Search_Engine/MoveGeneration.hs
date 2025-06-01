module MoveGeneration(sixAxis, freeFaces, kociembaMoves, notBlockedMoves) where

import Moves
import qualified Data.Set as S
import Bandaged

-- | Returns the moves achievable with 6 faces (R, R', R2, U ...)
sixAxis :: [Turn]
sixAxis = casualUnzip (map (casualZip [1 .. 3]) [R .. ] )

-- | Returns the possible moves of Kociemba 2nd step (R2, L2, F2, B2, U, D...)
kociembaMoves :: [Turn]
kociembaMoves = casualUnzip (qt ++ ht)
    where
        qt = map (casualZip [1 .. 3]) [U, D]
        ht = map (casualZip [2]) [R, L, F, B]

-- | Returns the layers that can be moved (not blocked) in a bandaged cube
notBlockedMoves :: BandagedCube -> [Turn]
notBlockedMoves bc = freeFaces (movableFaces bc)

-- | Returns the possible moves given a list of layers
freeFaces :: [Face] -> [Turn]
freeFaces xs = casualUnzip (map (casualZip [1..3]) xs)

casualZip :: [Int] -> Face -> (Face, [Int])
casualZip xs f = (f, xs)

casualUnzip :: [(Face, [Int])] -> [Turn]
casualUnzip xs = concat (map indivFace xs)
    where
        indivFace :: (Face, [Int]) -> [Turn]
        indivFace = ( \(f,numsList) -> [Turn (f, n) | n <- numsList] )

-- | Returns the faces that can be moved
movableFaces :: BandagedCube -> [Face]
movableFaces bCube = S.toList (S.difference (S.fromList [R, U, F, L, D, B]) allBlockedFaces)
    where
        allRestr = restrictions bCube
        allBlockedFaces = S.unions (S.map (facesBlockedByBlock) allRestr)

        --Returns the layers that each block is blocking (when is over >1 centers)
        facesBlockedByBlock :: S.Set Int -> S.Set Face
        facesBlockedByBlock xs
            | (isBlockingCenters) = S.map (centerToLayer) centers
            | otherwise = S.empty
            where
                centers = S.filter (>= 48) xs
                isBlockingCenters = (length centers) >= 2

        centerToLayer :: Int -> Face
        centerToLayer 48 = U
        centerToLayer 49 = F
        centerToLayer 50 = R
        centerToLayer 51 = B
        centerToLayer 52 = L
        centerToLayer 53 = D
        centerToLayer _ = N
