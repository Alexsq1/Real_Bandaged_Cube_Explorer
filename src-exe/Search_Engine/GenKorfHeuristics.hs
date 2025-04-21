module GenKorfHeuristics(lookupAll, cornersVector, edgesFstVector, edgesSndVector) where

import qualified Data.Set as S
import Data.Maybe
import Data.Word(Word8)
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV
import Control.Monad.ST
import Control.Monad(forM_)

import IndexHeuristics
import Bandaged
import Moves
import InputBandagedCube(newSolvedBandagedCube)


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

type Vector8 = V.Vector Word8
type SetVisitedKeys = S.Set Int

-- | Generate a pattern database of corners from a state to depth n
cornersVector :: BandagedCube                                   -- ^ Initial state (solved recommended)
                -> Word8                                        -- ^ Maximum depth
                -> Vector8
cornersVector bc n = applyChangesMV 88179840 (n+1) ch
    where
        ch = bfsStoreChanges cornersKey n [R .. ] bc

--To be improved with only the movable faces in a bandaged

-- | Generate a pattern database of the first 6 edges from a state to depth n
edgesFstVector :: BandagedCube                                  -- ^ Initial state (solved recommended)
                -> Word8                                          -- ^ Maximum depth
                -> Vector8
edgesFstVector bc n = applyChangesMV 42577920 (n+1) ch
    where
        ch = bfsStoreChanges edgesKeyFst n [R .. ] bc

-- | Generate a pattern database of the last 6 edges from a state to depth n
edgesSndVector :: BandagedCube                                  -- ^ Initial state (solved recommended)
                -> Word8                                        -- ^ Maximum depth
                -> Vector8
edgesSndVector bc n = applyChangesMV 42577920 (n+1) ch
    where
        ch = bfsStoreChanges edgesKeySnd n [R .. ] bc

-- | Make the inmutable vector (with mutable operations) 
applyChangesMV :: Int                                           -- ^ Size
                -> Word8                                        -- ^ Maximum depth
                -> [(Int, Word8)]                               -- ^ Changes
                -> Vector8

applyChangesMV size defaultDepth changes = runST $ do
    mv <- MV.replicate size defaultDepth
    myUpdate mv changes
    V.unsafeFreeze mv

myUpdate :: MV.MVector s Word8 -> [(Int, Word8)] -> ST s ()
myUpdate v changes = forM_ changes $ (\(i, value) -> MV.write v i value)

bfsStoreChanges :: (BandagedCube -> Int) -> Word8 -> [Face] -> BandagedCube -> [(Int, Word8)]
bfsStoreChanges kGen maxDepth faces ini = bfs kGen maxDepth [(0, N, ini)] faces S.empty []
    where
        bfs :: (BandagedCube -> Int) -> Word8 -> [(Word8, Face, BandagedCube)] -> [Face] -> SetVisitedKeys -> [(Int, Word8)] -> [(Int, Word8)]
        bfs _ _ [] _ _ acc = acc
        bfs kGen2 maxDepth2 ((depth, lastFace, bCube) : fifo) faces2 visit acc
            | depth > maxDepth2 = acc                                                                   --prune, finish search
            | S.member thisKey visit = bfs kGen2 maxDepth2 fifo faces2 visit acc                        --visited state
            | otherwise = bfs kGen2 maxDepth2 (fifo ++ newFifoFiltered) faces2 newSet newList           --keep iterating
            where 
                thisKey = kGen2 bCube
                newSet = S.insert thisKey visit
                newList = (thisKey, depth) : acc

                moves = [ (f, Turn(f, m)) | f <- faces2, m <- [1 .. 3], 
                    (axisOfFace f /= axisOfFace lastFace) || (f > lastFace),
                    validTurn bCube f]

                possibleStates = map (\(lstFace, move) -> (lstFace, tryToTurn bCube move)) moves
                possibleAccesibleStates = filter (isJust . snd) possibleStates
                --Maybe filter the visited states here could be more efficient

                newFifo = map (\(lstFace, justState) -> (1 + depth, lstFace, fromJust justState)) possibleAccesibleStates
                newFifoFiltered = filter (\(_, _, potBCVisited) -> S.notMember (kGen2 potBCVisited) visit) newFifo

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

stdVectors :: (Vector8, Vector8, Vector8)
stdVectors = (c, e1, e2)
    where
        maxDepth = 4
        c = cornersVector ini maxDepth
        e1 = edgesFstVector ini maxDepth
        e2 = edgesSndVector ini maxDepth
        ini = newSolvedBandagedCube

lookupPiece :: Int -> BandagedCube -> Word8
lookupPiece n bc = 
    let (c, e1, e2) = stdVectors
    in case n of
        0 -> (V.!) c (cornersKey bc)
        1 -> (V.!) e1 (edgesKeyFst bc)
        2 -> (V.!) e2 (edgesKeySnd bc)
        _ -> 25
