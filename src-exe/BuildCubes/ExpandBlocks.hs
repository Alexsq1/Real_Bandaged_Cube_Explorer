module ExpandBlocks(expandBlocks) where

import Moves
import Bandaged
import qualified Data.Set as S

-- | Expand all the blocks
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
    | otherwise = s0_53
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
        full = s0_53

-- | A set of numbers 0..53
s0_53 :: S.Set Int
s0_53 = S.fromList [0..53]
