module Bandaged(BandagedCube(..), solvedBC, deleteBlocks, tryToTurn, tryToExecuteAlg, unsafeExecutionAlg, divideTurn, allPieces) where

import Moves
import Cube
import Data.Maybe(isNothing, fromJust)
import Data.List(intercalate)

--import Data.Int (Int8)
import qualified Data.Set as S
import qualified Data.Vector as V

-- | A Bandaged Cube is a Cube with a set of sets of restrictions.
data BandagedCube = BandagedCube {stdCube :: Cube, restrictions :: S.Set (S.Set Int)} deriving Eq

instance Ord BandagedCube where
    compare (BandagedCube c1 _) (BandagedCube c2 _) = compare c1 c2

instance Show BandagedCube where
    show (BandagedCube cube setRestrictions) = (show cube) ++ "\n\n" ++ 
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
    | n < 24 = corners !! (n `div` 3)
    | n < 48 = edges !! ((n - 24) `div` 2)
    | otherwise = centers !! (n - 48)
    where
        corners = ["UFL", "UBL", "UBR", "UFR", "DFR", "DBR", "DBL", "DFL"]
        edges = ["UL", "UB", "UR", "UF", 
            "FL", "FR", "BR", "BL",
            "DF", "DR", "DB", "DL"]
        centers = ["U", "F", "R", "B", "L", "D"]


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
unsafeExecutionAlg bc (Algorithm moves) = (finalCube, Algorithm (xsMoves))
    where
        (finalCube, xsMoves) = moveByMove bc moves []
        moveByMove :: BandagedCube -> [Turn] -> [Turn] -> (BandagedCube, [Turn])
        moveByMove bcAux [] accMoves = (bcAux, accMoves)
        moveByMove bcAux (x:xs) accMoves
            | isNothing next = moveByMove bcAux xs accMoves
            | otherwise = moveByMove (fromJust next) xs (accMoves ++ [x])
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
        --newPerm = execTurn currCube currTurn

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

