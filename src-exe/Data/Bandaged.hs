module Bandaged(BandagedCube(..), tryToTurn, divideTurn, allPieces) where
    --export validTurn, newBandagedCube

import Moves
import Cube
import qualified Data.Set as S
import qualified Data.Vector as V

-- | A Bandaged Cube is a Cube with a set of sets of restrictions. 
data BandagedCube = BandagedCube {stdCube :: Cube, restrictions :: S.Set (S.Set Int)} deriving Show


-- | Makes a move when it is possible and does not break any block
tryToTurn :: BandagedCube -> Turn -> Maybe BandagedCube
tryToTurn bCube currTurn
    | validTurn bCube f = Just (BandagedCube {stdCube = newPerm, restrictions = restr})
    | otherwise = Nothing
    where
        (BandagedCube currCube restr) = bCube
        (Turn(f, _)) = currTurn
        newPerm = currCube <> (permOfTurn currTurn)

--Checks is a Turn does not break any block
validTurn :: BandagedCube -> Face -> Bool
validTurn bCube face = and (boolsAllBlocks)
    where
        allRestr = restrictions bCube
        --checkOneBlock :: S.Set Int -> Bool
        checkOneBlock = turnPreserveBlock bCube face
        boolsAllBlocks = S.map (checkOneBlock) allRestr

turnPreserveBlock :: BandagedCube -> Face -> S.Set Int -> Bool
turnPreserveBlock (BandagedCube cubeState _) face block = (S.disjoint block s1Real) || (S.disjoint block s2Real)
    where
        (s1, s2) = divideTurn face
        s1RealVec = slicePieces cubeState (S.toList s1)
        s2RealVec = slicePieces cubeState (S.toList s2)
        s1Real = (S.fromList . V.toList) s1RealVec
        s2Real = (S.fromList . V.toList) s2RealVec
        --a bit better if vector was not imported, and cube has its own function of slicing
        



-- | Given a face, returns a tuple with the pieces afected and not afected respectively.
divideTurn :: Face -> (S.Set Int, S.Set Int)
divideTurn m = (piecesAfected m, piecesNotAfected m)

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

-- | A set of numbers 0..53
allPieces :: S.Set Int
allPieces = S.fromList [0..53]