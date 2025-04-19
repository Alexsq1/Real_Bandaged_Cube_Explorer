module GenKorfHeuristics(lookupAll, cornersVector, edgesFstVector, edgesSndVector) where

import IndexHeuristics
import Bandaged
import Moves
import InputBandagedCube(newSolvedBandagedCube)

import qualified Data.Vector.Unboxed as V
import qualified Data.Set as S
import Data.Maybe

import Data.Word(Word8)

import qualified Data.Vector.Unboxed.Mutable as MV
import Control.Monad.ST
import Control.Monad(forM_)


--Max. Int: 536.870.912
--Max. Int: 2 ^63 -1 = 9.223.372.036.854.775.807

--Max. index of corners: 88.179.839:     (8! - 1) * 3⁷ + 3⁷ - 1  (last digit of orientation is not counted. 7 digits are included)
--State: [7,6,5,4,3,2,1,0] [2,2,2,2,2,2,(1)]
--D' U' F2 D B2 F2 R2 U' F' U B F U B U F' D' F D F

--Max. index of edges: 42.577.919              (prod [7, 12] - 1) * 2⁶ + 2⁶ -1   
--State: [11,10,9,8,7,6] [1,1,1,1,1,1]
--Generate: R' U2 B2 L R D2 B2 D' L U F D B R' B' L' B L' U' F'
--Generate: (R2 F2)3 (B2 L2)3 (R2 E2)3 superflip

--Valid with indexes of 26-27 bits (4 bytes)
--Each number: 0-20 (4 bits)


--To be changed to 1 byte ints (only vector)
type Vector8 = V.Vector Word8
type SetVisitedKeys = S.Set Int

cornersVector :: BandagedCube -> Vector8
cornersVector bc = applyChangesMV 88179840 ch
    where
        ch = bfsStoreChanges cornersKey [R .. ] bc
--To be improved with only the movable faces in a bandaged

edgesFstVector :: BandagedCube -> Vector8
edgesFstVector bc = applyChangesMV 42577920 ch
    where
        ch = bfsStoreChanges edgesKeyFst [R .. ] bc

edgesSndVector :: BandagedCube -> Vector8
edgesSndVector bc = applyChangesMV 42577920 ch
    where
        ch = bfsStoreChanges edgesKeySnd [R .. ] bc

applyChangesMV :: Int -> [(Int, Word8)] -> Vector8
applyChangesMV n changes = runST $ do
    mv <- MV.replicate n 21
    myUpdate mv changes
    V.unsafeFreeze mv

myUpdate :: MV.MVector s Word8 -> [(Int, Word8)] -> ST s ()
myUpdate v changes = forM_ changes $ (\(i, value) -> MV.write v i value)

bfsStoreChanges :: (BandagedCube -> Int) -> [Face] -> BandagedCube -> [(Int, Word8)]
bfsStoreChanges kGen faces ini = bfs kGen [(0, N, ini)] faces S.empty []
    where
        bfs :: (BandagedCube -> Int) -> [(Word8, Face, BandagedCube)] -> [Face] -> SetVisitedKeys -> [(Int, Word8)] -> [(Int, Word8)]
        bfs _ [] _ _ acc = acc
        bfs kGen2 ((depth, lastFace, bCube) : fifo) faces2 visit acc
            | depth > 4 = acc                                                       --prune, finish search
            | S.member thisKey visit = bfs kGen2 fifo faces2 visit acc              --visited state
            | otherwise = bfs kGen2 (fifo ++ newFifo) faces2 newSet newList         --keep iterating

            where 
                thisKey = kGen2 bCube
                newSet = S.insert thisKey visit
                newList = (thisKey, depth) : acc

                moves = [ (f, Turn(f, m)) | f <- faces2, m <- [1 .. 3], 
                    (axisOfFace f /= axisOfFace lastFace) || (f > lastFace),
                    validTurn bCube f]

                possibleStates = map (\(lstFace, move) -> (lstFace, tryToTurn bCube move)) moves
                possibleAccesibleStates = filter (isJust . snd) possibleStates
                newFifo = map (\(lstFace, justState) -> (1 + depth, lstFace, fromJust justState)) possibleAccesibleStates
                
--may be improved with list of visited sets, ordered by depths. Problem: editing 1 element. (Maybe vector boxed)
--            | depth > 0 && ((S.member thisKey lastLayer) || (S.member thisKey thisLayer)) = visit



lookupAll :: BandagedCube -> (Word8, Word8, Word8)
lookupAll bc = (lookupCorners bc, lookupFstEdges bc, lookupSndEdges bc)

lookupCorners :: BandagedCube -> Word8
lookupCorners = lookupPiece 0

lookupFstEdges :: BandagedCube -> Word8
lookupFstEdges = lookupPiece 1

lookupSndEdges :: BandagedCube -> Word8
lookupSndEdges = lookupPiece 2


--Ugly, strange failure
stdVectors :: (Vector8, Vector8, Vector8)
stdVectors = (c, e1, e2)
    where
        c = cornersVector ini
        e1 = edgesFstVector ini
        e2 = edgesSndVector ini
        ini = newSolvedBandagedCube

lookupPiece :: Int -> BandagedCube -> Word8
lookupPiece 0 bc = (V.!) c (cornersKey bc)
lookupPiece 1 bc = (V.!) e1 (edgesKeyFst bc)
lookupPiece 2 bc = (V.!) e2 (edgesKeySnd bc)
lookupPiece _ _ = 25
    where
        (c, e1, e2) = stdVectors

