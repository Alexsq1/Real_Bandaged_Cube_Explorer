module Bandaged(BandagedCube(..), solvedBC, deleteBlocks, tryToTurn, tryToExecuteAlg, validTurn, unsafeExecutionAlg, divideTurn) where

import Cube
import Moves
import qualified Data.Set as S
import Data.Maybe(isNothing, fromJust)
import Data.List(intercalate)

import Test.QuickCheck

-- | A Bandaged Cube is a Cube with a set of sets of restrictions.
data BandagedCube = BandagedCube {stdCube :: Cube, restrictions :: S.Set (S.Set Int)} deriving Eq

instance Ord BandagedCube where
    compare (BandagedCube c1 _) (BandagedCube c2 _) = compare c1 c2

instance Show BandagedCube where
    show (BandagedCube cube setRestrictions) = (show cube) ++ "\n" ++ (show listRestrictions) ++ "\n" ++ 
                        --(show listRestrictions) ++ "\n" ++
                        "BLOCKS: \n" ++
                        (intercalate "\n" (map unwords facesPieces))
        where
            listRestrictions = S.toList (S.map S.toList setRestrictions)
            notRepeatingPieces = map (filterEachBlock) listRestrictions
            facesPieces = (map . map) intToStrPiece notRepeatingPieces
    
            filterEachBlock :: [Int] -> [Int]
            filterEachBlock = filter (\x -> (x < 24 && (x `mod` 3 == 0)) || (x >= 24 && x < 48 && (x `mod` 2 == 0)) || (x >= 48))

intToStrPiece :: Int -> String
intToStrPiece n
    | n < 24 = cornersP !! (n `div` 3)
    | n < 48 = edgesP !! ((n - 24) `div` 2)
    | otherwise = centersP !! (n - 48)
    where
        cornersP = ["UFL", "UBL", "UBR", "UFR", "DFR", "DBR", "DBL", "DFL"]
        edgesP = ["UL", "UB", "UR", "UF", 
            "FL", "FR", "BR", "BL",
            "DF", "DR", "DB", "DL"]
        centersP = ["U", "F", "R", "B", "L", "D"]


solvedBC :: BandagedCube -> Bool
solvedBC (BandagedCube c _) = solved c

-- | Given a Bandaged Cube, returns the cube without blocks
deleteBlocks :: BandagedCube -> Cube
deleteBlocks bc = stdCube bc

-- | Execute an algorithm on a Bandaged Cube when it does not break any block
tryToExecuteAlg :: BandagedCube -> Algorithm -> Maybe BandagedCube
tryToExecuteAlg bc (Algorithm xsmoves) = moveByMove (Just bc) xsmoves
    where
        moveByMove :: Maybe BandagedCube -> [Turn] -> Maybe BandagedCube
        moveByMove Nothing _ = Nothing
        moveByMove bcAux [] = bcAux
        moveByMove (Just bcAux) (x:xs) = let next = tryToTurn bcAux x 
                                    in moveByMove next xs 

-- | Try to execute an algorithm on a Bandaged Cube. When a move is not valid, it skips the move. It returns the final state and the algorithm executed.
unsafeExecutionAlg :: BandagedCube -> Algorithm -> (BandagedCube, Algorithm)
unsafeExecutionAlg bc (Algorithm moves) = (finalCube, Algorithm (reverse xsMoves))
    where
        (finalCube, xsMoves) = moveByMove bc moves []
        moveByMove :: BandagedCube -> [Turn] -> [Turn] -> (BandagedCube, [Turn])
        moveByMove bcAux [] accMoves = (bcAux, accMoves)
        moveByMove bcAux (x:xs) accMoves
            | isNothing next = moveByMove bcAux xs accMoves
            | otherwise = moveByMove (fromJust next) xs (x : accMoves)
            where
                next = tryToTurn bcAux x

-- | Makes a move when it is possible and does not break any block
tryToTurn :: BandagedCube -> Turn -> Maybe BandagedCube
tryToTurn bCube currTurn
    | validTurn bCube f = Just (BandagedCube {stdCube = newPerm, restrictions = restr})
    | otherwise = Nothing
    where
        (BandagedCube currCube restr) = bCube
        (Turn(f, _)) = currTurn
        newPerm = currCube <> (permOfTurn currTurn)

-- | Checks if a Face would not break any block
validTurn :: BandagedCube -> Face -> Bool
validTurn bCube face = all (checkOneBlock) allRestr
    where
        allRestr = restrictions bCube
        checkOneBlock = turnPreserveBlock bCube face

turnPreserveBlock :: BandagedCube -> Face -> S.Set Int -> Bool
turnPreserveBlock (BandagedCube cubeState _) face block = (S.disjoint block s1Real) || (S.disjoint block s2Real)
    where
        (s1, s2) = divideTurn face  
        s1Real = S.fromList (slicePieces (S.toList s1) cubeState)
        s2Real = S.fromList (slicePieces (S.toList s2) cubeState)

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
piecesNotAfected bm = S.difference s0_53 (piecesAfected bm)
    where
        s0_53 = S.fromList [0..53]


instance Arbitrary BandagedCube where
    arbitrary = do
        alg <- arbitrary
        let bcs = BandagedCube {stdCube = newCubeFromList [0 .. 53], 
            restrictions = S.singleton (S.empty)}
        return (fromJust (tryToExecuteAlg bcs alg))
