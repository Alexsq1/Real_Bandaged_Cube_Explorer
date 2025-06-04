module Cube (Cube(..), newCubeFromList, solved, slicePieces, corners, edges, centers, allPieces) where

import Data.Group
import qualified Data.Vector.Unboxed as V
import Data.List(sortBy)

import Test.QuickCheck

newtype Cube = Cube (V.Vector Int) deriving (Show, Eq)

-- | Creates a cube by a 0-53 stickers list
newCubeFromList :: [Int] -> Cube
newCubeFromList xs = Cube $ V.fromList xs

instance Semigroup Cube where
    (Cube v1) <> (Cube v2) = Cube(V.unsafeBackpermute v1 v2)

instance Monoid Cube where
    mempty = Cube (V.fromList [0..53])

-- | Checks if a cube is solved
solved :: Cube -> Bool
solved = (== mempty)

instance Group Cube where
    invert (Cube xs) = Cube(V.fromList (invert_perm (V.toList xs)))

invert_perm :: [Int] -> [Int]
invert_perm xs = map fst tups_ord
    where
        neutral = [0 .. ]
        tups = zip (neutral) xs 
        tups_ord = sortBySnd tups

        sortBySnd :: Ord b => [(a,b)] -> [(a,b)]
        sortBySnd = sortBy (\(_, s1) (_, s2) -> compare s1 s2)

instance Ord Cube where
    compare (Cube v1) (Cube v2) = compare v1 v2

-- | Given a cube and a list, returns a list with all the pieces of the position given. 
slicePieces :: [Int] -> Cube -> [Int]
slicePieces ys (Cube xs) = V.toList (V.backpermute xs (V.fromList ys))

-- | Returns a list of the corner pieces
corners :: Cube -> [Int]
corners = slicePieces [0 .. 23]

-- | Returns a list of the edges pieces
edges :: Cube -> [Int]
edges = slicePieces [24 .. 47]

-- | Returns a list of the center pieces
centers :: Cube -> [Int]
centers = slicePieces [48 .. 53]

-- | Returns a list with all of the pieces
allPieces :: Cube -> [Int]
allPieces (Cube xs) = V.toList xs

instance Arbitrary Cube where
    arbitrary = do
        xs <- shuffle [0..53]
        return $ newCubeFromList xs
    --Random stickers permutations. Can be improved with real perms
