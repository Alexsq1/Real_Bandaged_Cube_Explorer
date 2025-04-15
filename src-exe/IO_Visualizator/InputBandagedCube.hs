module InputBandagedCube(newBandagedCube) where

import Moves
import Cube
import Bandaged
import qualified Data.Set as S

-- | Creates a new Bandaged Cube, with a standard cube and the restrictions
newBandagedCube :: Cube -> [[Int]] -> BandagedCube
newBandagedCube cubeOrigin blocks = BandagedCube {stdCube = cubeOrigin, restrictions = postProcessedBlocks}
    where
        blocksSet = doubleListToDoubleSet blocks
        postProcessedBlocks = (expandBlocks . canonicSets) blocksSet

--All auxiliary from here

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

pieceIntersections :: (Foldable f) => f (S.Set Int) -> S.Set Int
pieceIntersections = foldl' S.intersection (full)
    where
        full = allPieces
