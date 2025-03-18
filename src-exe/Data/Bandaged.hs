module Bandaged(newBandagedCube) where
    --export validTurn, newBandagedCube

import AlgToCube
import Moves
import Cube
import qualified Data.Set as S

data BandagedCube = BC {cubo :: Cube, restrictions :: S.Set (S.Set Int)} deriving Show

newBandagedCube :: Cube -> [[Int]] -> BandagedCube
newBandagedCube cubeOrigin blocks = BC {cubo = cubeOrigin, restrictions = postProcessedBlocks}
    where
        postProcessedBlocks = (expandBlocks . canonicSets) blocks

canonicSets :: S.Set (S.Set Int) -> S.Set (S.Set Int)
canonicSets ss = S.fromList (genUnions (S.toList ss) [])

genUnions :: [S.Set Int] -> [S.Set Int] -> [S.Set Int]
genUnions [] xs = xs
genUnions (s0:rest) temp = genUnions rest (combined : notCombining)
    where
        notCombining = filter (S.disjoint s0) temp
        toComb = filter (not. S.disjoint s0) temp
        combined = S.unions (s0: toComb)

--Bloques y giros

expandBlocks :: S.Set (S.Set Int) -> S.Set (S.Set Int)
expandBlocks setOfBlocks = S.map expand1Block setOfBlocks

expand1Block :: S.Set Int -> S.Set Int
expand1Block block = pieceIntersections (expansions)
    where
        expansions = map (expandBlockTurn block) [R,U,L,D,B,F]
    --calcular con cada giro, e intersección

expandBlockTurn :: S.Set Int -> BasicMove -> S.Set Int
expandBlockTurn block move
    | (turnPreserveBlock move block) = rightSubSet
    | otherwise = allPieces
    where
        (xs1, xs2) = divideTurn move
        rightSubSet = if (S.disjoint block xs2) then xs1 else xs2


--Calcula si un giro preservaría un bloque concreto
--TODO : Se calcula en base a la posición en estado resuelto. Habría que hacerlo con una posición del cubo arbitraria
turnPreserveBlock :: BasicMove -> S.Set Int -> Bool
turnPreserveBlock bm block = (S.disjoint block s1) || (S.disjoint block s2)
    where
        (s1, s2) = divideTurn bm



--Not used

--Revisa si un giro no rompe ningún bloque
validTurn :: BandagedCube -> BasicMove -> Bool
validTurn bCube move = and (S.map (turnPreserveBlock move) allRestr)
    where
        allRestr = restrictions bCube

movableFaces :: BandagedCube -> S.Set BasicMove
movableFaces bCube = S.difference (S.fromList [R, U, F, L, D, B]) allBlockedFaces
    where
        allRestr = restrictions bCube
        allBlockedFaces = S.unions (S.map (blockFaces) allRestr)

--Devuelve las capas que bloquea un bloque (>1 centros)
blockFaces :: S.Set Int -> S.Set BasicMove
blockFaces xs
    | (blockingCenters) = S.map (centerToLayer) centers
    | otherwise = S.fromList []
    where
        centers = S.filter (>= 48) xs --Número de centros de un bloque
        blockingCenters = (length centers) >= 2 --Bool, si bloquea varios

--aux
allPieces :: S.Set Int
allPieces = S.fromList [0..53]

piecesAfected :: BasicMove -> S.Set Int
piecesAfected R = S.fromList [6,7,8,9,10,11,12,13,14,15,16,17,28,29,34,35,36,37,42,43,50]
piecesAfected U = S.fromList [9,10,11,0,1,2,3,4,5,6,7,8,30,31,24,25,26,27,28,29,48]
piecesAfected F = S.fromList [22,23,21,2,0,1,10,11,9,14,12,13,33,32,41,40,31,30,35,34,49]
piecesAfected L = S.fromList [5,3,4,19,20,18,23,21,22,1,2,0,38,39,24,25,46,47,32,33,52]
piecesAfected D = S.fromList [21,22,23,12,13,14,15,16,17,18,19,20,46,47,40,41,42,43,44,45,53]
piecesAfected B = S.fromList [8,6,7,16,17,15,20,18,19,4,5,3,37,36,45,44,27,26,39,38,51]
piecesAfected _ = S.fromList []

piecesNotAfected :: BasicMove -> S.Set Int
piecesNotAfected bm = S.difference allPieces (piecesAfected bm)

divideTurn :: BasicMove -> (S.Set Int, S.Set Int)
divideTurn m = (piecesAfected m, piecesNotAfected m)

centerToLayer :: Int -> BasicMove
centerToLayer 48 = U
centerToLayer 49 = F
centerToLayer 50 = R
centerToLayer 51 = B
centerToLayer 52 = L
centerToLayer 53 = D

pieceIntersections :: (Foldable f) => f (S.Set Int) -> S.Set Int
pieceIntersections = foldl' S.intersection (full)
    where
        full = allPieces


--preprocesar bloques: piezas adyacentes entre sí.
--misma pieza en varios bloques: hacer unión.

--Piezas adyacentes: en un bloque, al menos uno lo rompería o impediría.

--Un bloque siempre tiene alguna arista. Puede incluir esquinas o centros (mínimo uno)