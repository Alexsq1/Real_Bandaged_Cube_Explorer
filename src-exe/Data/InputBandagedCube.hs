module InputBandagedCube where

import Moves
import Cube
import qualified Data.Set as S



module Bandaged(newBandagedCube) where
    --export validTurn, newBandagedCube

import Moves
import Cube
import qualified Data.Set as S

data BandagedCube = BandagedCube {stdCube :: Cube, restrictions :: S.Set (S.Set Int)} deriving Show

newBandagedCube :: Cube -> [[Int]] -> BandagedCube
newBandagedCube cubeOrigin blocks = BandagedCube {stdCube = cubeOrigin, restrictions = postProcessedBlocks}
    where
        blocksSet = doubleListToDoubleSet blocks
        postProcessedBlocks = (expandBlocks . canonicSets) blocksSet

doubleListToDoubleSet :: Ord a => [[a]] -> S.Set (S.Set a)
doubleListToDoubleSet xs = S.fromList (map S.fromList xs)

-- | Join (union) the sets that are not disjoint.
canonicSets :: S.Set (S.Set Int) -> S.Set (S.Set Int)
canonicSets ss = genUnions (S.toList ss) (doubleListToDoubleSet [[]])
--Maybe it can be improved with folds, and not convert to lists back again

---- Auxiliar of canonic set. Takes a list of sets, makes the unions recursively
genUnions :: [S.Set Int] -> S.Set (S.Set Int) -> S.Set (S.Set Int)
genUnions [] xs = S.delete (S.empty) xs 
genUnions (s0:rest) temp = genUnions rest (S.insert unitedSet notCombining)
    where
        (notCombining, toComb) = S.partition (S.disjoint s0) temp
        unitedSet = S.unions (S.insert s0 toComb)
        --Generates the union of the non-disjoint sets



--All auxiliary from here

--Tries to expand all the blocks
expandBlocks :: S.Set (S.Set Int) -> S.Set (S.Set Int)
expandBlocks setOfBlocks = S.map expand1Block setOfBlocks

--Expand a single block, with all the possible turns
expand1Block :: S.Set Int -> S.Set Int
expand1Block block = pieceIntersections (expansions)
    where
        expansions = map (expandBlockTurn block) [R,U,L,D,B,F]
    
--Given a block and a Face, returns the expanded block
expandBlockTurn :: S.Set Int -> Face -> S.Set Int
expandBlockTurn block move
    | isBlockPreserved = rightSubSet
    | otherwise = allPieces
    where
        (xs1, xs2) = divideTurn move
        --Every turn divides the cube in 2 sets: pieces affected and not affected.
        --The block is preserved when the full block is in one of the 2 sub-partitions
        isBlockPreserved = (S.disjoint block xs1) || (S.disjoint block xs2)
        
        --There are going to be intersections. Must return the proper sub-partition
        rightSubSet = if (S.disjoint block xs2) then xs1 else xs2

--Keep in mind: it returns the pieces of the solved state. 
--Maybe it is better to do that in the scrambled state.
--Maybe it is better to preprocess the cube, and detect it in the checking state 

--Given a face, returns the pieces afected and not afected.
divideTurn :: Face -> (S.Set Int, S.Set Int)
divideTurn m = (piecesAfected m, piecesNotAfected m)


pieceIntersections :: (Foldable f) => f (S.Set Int) -> S.Set Int
pieceIntersections = foldl' S.intersection (full)
    where
        full = allPieces


allPieces :: S.Set Int
allPieces = S.fromList [0..53]

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


----preprocesar bloques: piezas adyacentes entre sí.
----misma pieza en varios bloques: hacer unión.
----Piezas adyacentes: en un bloque, al menos uno lo rompería o impediría.
----Un bloque siempre tiene alguna arista. Puede incluir esquinas o centros (mínimo uno)