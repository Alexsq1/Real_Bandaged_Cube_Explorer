module GenKorfHeuristics(cornersVector, edgesFstVector, edgesSndVector) where

import IndexHeuristics
import Cube
import Bandaged
import Moves

import qualified Data.Vector.Unboxed as V
import qualified Data.Set as S
import Data.Maybe

--import Data.Word8 as w8

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
type Vector8 = V.Vector Int
type SetVisitedKeys = S.Set Int

initialStateCorners :: Vector8
initialStateCorners = V.replicate (88179840 :: Int) 21

initialStateEdges :: Vector8
initialStateEdges = V.replicate (42577920 :: Int) 21

cornersVector :: BandagedCube -> Vector8
cornersVector = createVector initialStateCorners cornersKey [R .. ]
--To be improved with only the movable faces in a bandaged

edgesFstVector :: BandagedCube -> Vector8
edgesFstVector = createVector initialStateEdges edgesKeyFst [R .. ]

edgesSndVector :: BandagedCube -> Vector8
edgesSndVector = createVector initialStateEdges edgesKeySnd [R .. ]

createVector :: Vector8 -> (BandagedCube -> Int) -> [Face] -> BandagedCube -> Vector8
createVector iniVector kGen faces ini = bfs kGen [(0, N, ini)] faces S.empty iniVector
--    where
bfs :: (BandagedCube -> Int) -> [(Int, Face, BandagedCube)] -> [Face] -> SetVisitedKeys -> Vector8 -> Vector8
bfs _ [] _ _ iniVec = iniVec
bfs kGen2 ((depth, lastFace, bCube) : fifo) faces2 visit iniVec
    | depth >= 3 = iniVec
    | S.member thisKey visit = bfs kGen2 fifo faces2 visit iniVec
    | otherwise = bfs kGen2 (fifo ++ newFifo) faces2 newSet newVector

    where 
        thisKey = kGen2 bCube
        newSet = S.insert thisKey visit
        newVector = (V.//) iniVec [(thisKey, depth)]
        
        moves = [ (f, Turn(f, m)) | f <- faces2, m <- [1 .. 3], 
            f /= lastFace, 
            (axisOfFace f /= axisOfFace lastFace) || ((axisOfFace f == axisOfFace lastFace) && (f > lastFace)),
            validTurn bCube f]

        --Turn(thFace, _) : _ = fs

        possibleStates = map (\(lstFace, move) -> (lstFace, tryToTurn bCube move)) moves
        possibleAccesibleStates = filter (isJust . snd) possibleStates
        newFifo = map (\(lstFace, justState) -> (1 + depth, lstFace, fromJust justState)) possibleAccesibleStates

        --newFifo = map ((\x -> (1 + depth , x)) . fromJust . (tryToTurn bCube)) fs
        --newFifo = map (\(ff, move) -> () . fromJust . (tryToTurn bCube move) ) fs
        --dangerous to do fromJust. Won't work with bandageds
        
-- to be optimized with canonic sequences
--careful when inserting to the fifo seqs of length n+1 and n+2. Must be sorted in length (update when first ocurrence)
--may be improved with list of visited sets, ordered by depths. Problem: editing 1 element. (Maybe vector boxed)
--            | depth > 0 && ((S.member thisKey lastLayer) || (S.member thisKey thisLayer)) = visit
