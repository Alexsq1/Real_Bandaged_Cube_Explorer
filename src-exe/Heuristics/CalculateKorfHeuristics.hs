module CalculateKorfHeuristics(stdVectors) where

--import Bandaged
import Cube(Cube(..))
import Moves(Turn(..), Face(..), applyMove, axisOfFace)
import CubeCreator(newSolvedCube)
import IndexHeuristics

import qualified Data.Set as S
import Data.Maybe(fromJust)
import Data.Word(Word8)

import Data.PSQueue as PS
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV
import Control.Monad.ST
import Control.Monad(forM_)


--import Debug.Trace (trace, traceShow)

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

-- | Alias for Word8 Vectors
type Vector8 = V.Vector Word8

-- | Calculates a vector with the depths of a pattern database
stdVectors :: Word8 -> (Vector8, Vector8, Vector8)
stdVectors maxDepth = (c, e1, e2)
    where
        c = cornersVector maxDepth
        e1 = edgesFstVector maxDepth
        e2 = edgesSndVector maxDepth

-- | Generate a pattern database of corners from a state to depth n
cornersVector :: Word8                                        -- ^ Maximum depth
                -> Vector8
cornersVector n = applyChangesMV 88179840 (n+1) ch
    where
        ch = bfsStoreChanges cornersKey n [R .. ] newSolvedCube

-- | Generate a pattern database of the first 6 edges from a state to depth n
edgesFstVector :: Word8                                          -- ^ Maximum depth
                -> Vector8
edgesFstVector n = applyChangesMV 42577920 (n+1) ch
    where
        ch = bfsStoreChanges edgesKeyFst n [R .. ] newSolvedCube

-- | Generate a pattern database of the last 6 edges from a state to depth n
edgesSndVector :: Word8                                        -- ^ Maximum depth
                -> Vector8
edgesSndVector n = applyChangesMV 42577920 (n+1) ch
    where
        ch = bfsStoreChanges edgesKeySnd n [R .. ] newSolvedCube

-- | Make the inmutable vector (with mutable operations) 
applyChangesMV :: Int                                           -- ^ Size
                -> Word8                                        -- ^ Maximum depth
                -> [(Int, Word8)]                               -- ^ Changes
                -> Vector8

applyChangesMV sizeV defaultDepth changes = runST $ do
    mv <- MV.replicate sizeV defaultDepth
    myUpdate mv changes
    V.unsafeFreeze mv

myUpdate :: MV.MVector s Word8 -> [(Int, Word8)] -> ST s ()
myUpdate v changes = forM_ changes $ (\(i, value) -> MV.write v i value)

newtype GenerationState = GenerationState (Int, Face, Cube)
--(Key, LastFace, BCube)

instance Eq GenerationState where 
    (GenerationState (key1, _, _)) == (GenerationState (key2, _, _)) = key1 == key2

instance Ord GenerationState where
    compare (GenerationState (key1, _, _)) (GenerationState (key2, _, _)) = compare key1 key2

instance Show GenerationState where
    show (GenerationState (k, _, _)) = '#' : (show k)
--PQS of genState Word8. Values are of GenState, Word8 are the priorities, depth


type SetVisitedKeys = S.Set Int

bfsStoreChanges :: (Cube -> Int) -> Word8 -> [Face] -> Cube -> [(Int, Word8)]
bfsStoreChanges kGen maxDepth faces initBC = bfs kGen maxDepth (PS.singleton gs0 0) faces S.empty S.empty []
    where
        gs0 = GenerationState (kGen initBC, N, initBC)

bfs ::  (Cube -> Int) -> Word8 
    -> PS.PSQ GenerationState Word8 -> [Face] 
    -> SetVisitedKeys -> SetVisitedKeys -> [(Int, Word8)] 
    -> [(Int, Word8)]
bfs kGen maxDepth pq faces visited onceEnqueued acc
    | PS.null pq = 
        --trace ("Ended the alg, recieved empty pq") $
        acc                                                      --empty generation, maybe not happening
    
    | isRepeated = 
        --trace ("Visited state: " ++ (show thisKey)) $
        bfs kGen maxDepth pqNoMin faces visited onceEnqueued acc              --repeated element
    
    | currDepth > maxDepth = 
        --trace ("First surpass: " ++ show thisKey ++ ", at depth " ++ show currDepth) $
        acc                                            --1st surpass, finished

    | currDepth == maxDepth = bfs kGen maxDepth pqNoMin faces nextVSet (S.delete thisKey onceEnqueued) newChanges    --Only check your case, not adding elements
    | otherwise = 
        --trace ("Normal, recieved " ++ show thisKey ++ " state at depth " ++ show currDepth ++ ", adding " ++ show nextGS ++ "\n") $
        bfs kGen maxDepth nextPQ faces nextVSet nextEnq newChanges      --Iterate

    where
        --Comprobations
        (thisGenState PS.:-> currDepth , pqNoMin) = fromJust (PS.minView pq)
        GenerationState (thisKey, _, _) = thisGenState
        isRepeated = S.member thisKey visited

        --Generation of next layer
        infListNextDepth = (repeat (1 + currDepth))
        nextGS = nextLayerNonRepeating kGen thisGenState faces visited onceEnqueued
        nextPQ = insertList (zip nextGS infListNextDepth) pqNoMin

        nextVSet = S.insert thisKey visited
        newChanges = (thisKey , currDepth) : acc

        keysEnq = map (\(GenerationState(k, _, _)) -> k) nextGS
        nextEnq = S.union (S.delete thisKey onceEnqueued) (S.fromList keysEnq)

insertList :: (Ord k, Ord p) => [(k , p)] -> PSQ k p -> PSQ k p
insertList [] pq = pq
insertList ((k , p):xs) pq = PS.insert k p (insertList xs pq)

nextLayerNonRepeating :: (Cube -> Int)
                        -> GenerationState -> [Face] 
                        -> SetVisitedKeys -> SetVisitedKeys -> [GenerationState]
                        
nextLayerNonRepeating kGen (GenerationState(_, lastFace, bCube)) faces visited onceEnqueued = newStatesFiltered
    where
        moves = [ (f, Turn(f, m)) | f <- faces, m <- [1 .. 3], (axisOfFace f /= axisOfFace lastFace) || (f > lastFace)]
        --possibleAccesibleStates = [(lf, tryToTurn bCube m) | (lf, m) <- moves, isJust (applyTurn bCube m)]
        possibleAccesibleStates = [(lf, applyMove bCube m) | (lf, m) <- moves]

        newStatesFiltered = [  GenerationState (kGen bc, lf, bc) | 
                            (lf, bc) <- possibleAccesibleStates, 
                            S.notMember (kGen bc) visited, 
                            S.notMember (kGen bc) onceEnqueued ]
