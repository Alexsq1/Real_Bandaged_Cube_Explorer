module Cube (Cube(..), newCubeFromList, solved, slicePieces) where

import Data.Group
--import Data.List
import qualified Data.Vector as V
import Utils

{- Implementation of a Rubik's Cube as a group.
Numbering the 54 stickers
-}

--A cube is a list of numbers 0-53. Maybe, use base 64 in the future

newtype Cube = Cube (V.Vector Int) deriving (Show, Eq)

--Todo: user-friendly user-experience

-- | Creates a cube by a 0-53 stickers list
newCubeFromList :: [Int] -> Cube
newCubeFromList xs = Cube $ V.fromList xs

instance Semigroup Cube where
    (Cube v1) <> (Cube v2) = Cube(V.backpermute v1 v2)

instance Monoid Cube where
    mempty = Cube (V.fromList [0..53])
    --Be careful when changing a representation

-- | Checks if a cube is solved
    
solved :: Cube -> Bool
solved = (== mempty)
--Be careful if rotations are allowed

instance Group Cube where
    invert (Cube xs) = Cube(V.fromList (invert_perm (V.toList xs)))

--try to hide
invert_perm :: [Int] -> [Int]
invert_perm xs = map fst tups_ord
    where
        neutral = [0 .. (length xs - 1)]
        tups = zip (neutral) xs 
        tups_ord = sort_by_snd tups

instance Ord Cube where
    compare (Cube v1) (Cube v2) = compare v1 v2

-- | Given a cube and a list, returns a vector with the pieces of the position given. 
slicePieces :: Cube -> [Int] -> V.Vector Int
slicePieces (Cube xs) ys = V.backpermute xs (V.fromList ys)