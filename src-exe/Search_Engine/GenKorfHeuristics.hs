module GenKorfHeuristics(lookupAll, cornersVector, edgesFstVector, edgesSndVector) where

import qualified Data.Set as S
import Data.Maybe(isJust, fromJust)
import Data.Word(Word8)

import Bandaged
import Moves
import InputBandagedCube(newSolvedBandagedCube)
import IndexHeuristics

import Data.PSQueue as PS
import qualified Data.Vector.Unboxed as V
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

--(Key, LastFace, BCube)
newtype GenerationState = GenerationState (Int, Face, BandagedCube)

instance Eq GenerationState where 
    (GenerationState (key1, _, _)) == (GenerationState (key2, _, _)) = key1 == key2

instance Ord GenerationState where
    compare (GenerationState (key1, _, _)) (GenerationState (key2, _, _)) = compare key1 key2

--PQS of genState Word8. Values are of GenState, Word8 are the priorities, depth 

bfsStoreChanges :: (BandagedCube -> Int) -> Word8 -> [Face] -> BandagedCube -> [(Int, Word8)]
bfsStoreChanges kGen maxDepth faces initBC = bfs kGen maxDepth (PS.singleton gs0 0) faces S.empty []
    where
        gs0 = GenerationState (kGen initBC, N, initBC)

--Has a bug, not reaching 1-move states
bfs ::  (BandagedCube -> Int) -> Word8 
    -> PS.PSQ GenerationState Word8 -> [Face] 
    -> SetVisitedKeys -> [(Int, Word8)] 
    -> [(Int, Word8)]
bfs kGen maxDepth pq faces visited acc
    | PS.null pq = acc                                                      --empty generation, maybe not happening
    | isRepeated = bfs kGen maxDepth pqNoMin faces visited acc              --repeated element
    | currDepth > maxDepth = acc                                            --1st surpass, finished
    | otherwise = bfs kGen maxDepth nextPQ faces nextVSet (newChanges)

    where
        (thisGenState PS.:-> currDepth , pqNoMin) = fromJust (PS.minView pq)
        GenerationState (thisKey, lastFace, thisBCube) = thisGenState
        isRepeated = S.member thisKey visited

        infListNextDepth = (repeat (1 + currDepth))
        nextGS = nextLayerNonRepeating kGen thisGenState faces visited
        nextPQ = insertList (zip nextGS infListNextDepth) pqNoMin

        newKeys = map (\(GenerationState(key, _, _)) -> key) nextGS
        nextVSet = S.union visited (S.fromList newKeys)
        newChanges = (thisKey , currDepth) : acc

insertList :: (Ord k, Ord p) => [(k , p)] -> PSQ k p -> PSQ k p
insertList [] pq = pq
insertList ((k , p):xs) pq = PS.insert k p (insertList xs pq)

nextLayerNonRepeating :: (BandagedCube -> Int)
                        -> GenerationState -> [Face] 
                        -> SetVisitedKeys -> [GenerationState]
nextLayerNonRepeating kGen (GenerationState(k, lastFace, bCube)) faces visited = newStatesFiltered
    where
        moves = [ (f, Turn(f, m)) | f <- faces, m <- [1 .. 3], 
            (axisOfFace f /= axisOfFace lastFace) || (f > lastFace),
            validTurn bCube f]
        possibleAccesibleStates = [(lf, tryToTurn bCube m) | (lf, m) <- moves , isJust (tryToTurn bCube m)]

        newStatesFiltered = [  GenerationState (kGen bc, lf, bc) | 
                    (lf, Just bc) <- possibleAccesibleStates , S.notMember (kGen bc) visited ]


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
        maxDepth = 2
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
