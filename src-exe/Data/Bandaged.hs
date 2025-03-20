module Bandaged(newBandagedCube) where
    --export validTurn, newBandagedCube

import Moves
import Cube
import qualified Data.Set as S

data BandagedCube = BandagedCube {stdCube :: Cube, restrictions :: S.Set (S.Set Int)} deriving Show


piecesAfected :: Face -> S.Set Int
piecesAfected R = S.fromList [6,7,8,9,10,11,12,13,14,15,16,17,28,29,34,35,36,37,42,43,50]
piecesAfected U = S.fromList [9,10,11,0,1,2,3,4,5,6,7,8,30,31,24,25,26,27,28,29,48]
piecesAfected F = S.fromList [22,23,21,2,0,1,10,11,9,14,12,13,33,32,41,40,31,30,35,34,49]
piecesAfected L = S.fromList [5,3,4,19,20,18,23,21,22,1,2,0,38,39,24,25,46,47,32,33,52]
piecesAfected D = S.fromList [21,22,23,12,13,14,15,16,17,18,19,20,46,47,40,41,42,43,44,45,53]
piecesAfected B = S.fromList [8,6,7,16,17,15,20,18,19,4,5,3,37,36,45,44,27,26,39,38,51]
piecesAfected _ = S.fromList []

piecesNotAfected :: Face -> S.Set Int
piecesNotAfected bm = S.difference allPieces (piecesAfected bm)


--tryToTurn :: BandagedCube -> Turn -> Maybe BandagedCube

--Unused by the moment
--turnPreserveBlock :: Face -> S.Set Int -> Bool
--turnPreserveBlock face block = (S.disjoint block s1) || (S.disjoint block s2)
--    where
--        (s1, s2) = divideTurn face
        --sreal = S.backpermute 


----Not used

----Revisa si un giro no rompe ningún bloque
--validTurn :: BandagedCube -> Face -> Bool
--validTurn bCube move = and (S.map (turnPreserveBlock move) allRestr)
--    where
--        allRestr = restrictions bCube
--



--movableFaces :: BandagedCube -> S.Set Face
--movableFaces bCube = S.difference (S.fromList [R, U, F, L, D, B]) allBlockedFaces
--    where
--        allRestr = restrictions bCube
--        allBlockedFaces = S.unions (S.map (blockFaces) allRestr)
--
----Devuelve las capas que bloquea un bloque (>1 centros)
--blockFaces :: S.Set Int -> S.Set Face
--blockFaces xs
--    | (blockingCenters) = S.map (centerToLayer) centers
--    | otherwise = S.fromList []
--    where
--        centers = S.filter (>= 48) xs --Número de centros de un bloque
--        blockingCenters = (length centers) >= 2 --Bool, si bloquea varios
--
----aux



--centerToLayer :: Int -> Face
--centerToLayer 48 = U
--centerToLayer 49 = F
--centerToLayer 50 = R
--centerToLayer 51 = B
--centerToLayer 52 = L
--centerToLayer 53 = D
--

