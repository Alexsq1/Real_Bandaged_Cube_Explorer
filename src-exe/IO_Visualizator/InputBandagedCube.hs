module InputBandagedCube(newBandagedCube, newSolvedBandagedCube, newSolvedCube) where

import Cube
import Bandaged
import ExpandBlocks(expandBlocks)
import qualified Data.Set as S

-- | Creates a standard cube
newSolvedCube :: Cube
newSolvedCube = newCubeFromList [0..53]

-- | Creates a solved Bandaged with no blocks
newSolvedBandagedCube :: BandagedCube
newSolvedBandagedCube = newBandagedCube (newCubeFromList [0 .. 53]) [[]]

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
