module CalculateKorfHeuristics(stdVectors) where

--import Bandaged
import Cube(Cube(..))
import Moves(Turn(..), Face(..), applyMove, axisOfFace)
import CubeCreator(newSolvedCube)
import IndexHeuristics
import LoadKorfHeuristics(loadVectors)

import qualified Data.Set as S
import Data.Maybe(fromJust)
import Data.Word(Word8)

import Data.PSQueue as PS
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV
import Control.Monad.ST
import Control.Monad(forM_)


--import Debug.Trace (trace, traceShow)


-- | Alias for Word8 Vectors
type Vector8 = V.Vector Word8
type HVector = (Word8, Vector8, Vector8, Vector8)

-- | Calculates a vector with the depths of a pattern database
stdVectors :: Word8 -> IO (HVector)
stdVectors currDepth = do
    (n1, cPrev, e1Prev, e2Prev) <- loadLastVector currDepth
    let c = cornersVector   currDepth cPrev
    let e1 = edgesFstVector currDepth e1Prev
    let e2 = edgesSndVector currDepth e2Prev
    return (currDepth, c, e1, e2)

-- | Reads the previous vector by accessing a file. If asked for 0, generates empty vector of corresponding length
loadLastVector :: Word8 -> IO (HVector)
loadLastVector n
    | n == 0 = pure (0, emptyVec 88179840, emptyVec 42577920, emptyVec 42577920)
    | otherwise = loadVectors (n-1)

-- | Generates empty vectors (all values are 255) of a given length
emptyVec :: Int -> Vector8
emptyVec n = V.fromList (replicate n 255)
--if goes fast enough, use it. If not:
{- emptyVec n = runST $ do
    mv <- MV.replicate n 255
    V.unsafeFreeze mv -}

-- | Generate a pattern database of corners from a state to depth n
cornersVector :: Word8                                        -- ^ Current depth
                -> Vector8                                    -- ^ Previous vector
                -> Vector8
cornersVector n pv = applyChangesMV 88179840 (n+1) pv ch
    where
        ch = bfsStoreChanges cornersKey n [R .. ] newSolvedCube
        --ch = nextLayer Corner n pv

-- | Generate a pattern database of the first 6 edges from a state to depth n
edgesFstVector :: Word8                                          -- ^ Current depth
                -> Vector8                                    -- ^ Previous vector
                -> Vector8
edgesFstVector n pv = applyChangesMV 42577920 (n+1) pv ch
    where
        ch = bfsStoreChanges edgesKeyFst n [R .. ] newSolvedCube

-- | Generate a pattern database of the last 6 edges from a state to depth n
edgesSndVector :: Word8                                        -- ^ Current depth
                -> Vector8                                    -- ^ Previous vector
                -> Vector8
edgesSndVector n pv = applyChangesMV 42577920 (n+1) pv ch
    where
        ch = bfsStoreChanges edgesKeySnd n [R .. ] newSolvedCube

-- | Make the inmutable vector (with mutable operations) 
applyChangesMV :: Int                                           -- ^ Size               (unnecesary)
                -> Word8                                        -- ^ Current depth      (unnecesary)
                -> Vector8                                    -- ^ Previous vector
                -> [(Int, Word8)]                               -- ^ Changes
                -> Vector8
applyChangesMV sizeV defaultDepth v changes = runST $ do
    --mv <- MV.replicate sizeV defaultDepth
    mv <- V.unsafeThaw v
    myUpdate mv changes
    V.unsafeFreeze mv

myUpdate :: MV.MVector s Word8 -> [(Int, Word8)] -> ST s ()
myUpdate v changes = forM_ changes $ (\(i, value) -> MV.write v i value)




data HPiece = Corner | Edge1 | Edge2 deriving (Eq, Show)

{- nextLayer :: HPiece                  -- ^ Type of piece that is being calculated
            -> Word8                 -- ^ Current depth
            -> Vector8               -- ^ Previous vector
            -> [(Int, Word8)] -}

{- 
    1: find indices of value (currD - 1)    (V.filter (\t -> snd t == cd-1) (V.zip (V.fromList[0..]) xs))
    2: recompose cubes of that value (recompose)
    3: apply all (18) moves to those cubes (concat, map)
    4: filter to only those that are (== currD)
    5: calc their key (map)
    6: store in changes term (zip (repeat currDepth))
-}

recompose :: HPiece -> Int -> Cube
recompose Corner n = keysToCube (n,0,0)
recompose Edge1 n = keysToCube (0,n,0)
recompose Edge2 n = keysToCube (0,0,n)
{- Future: not recomposing the whole cube, only specific pieces (efficiency) -}

--PENDING TO DELETE ALL FROM HERE

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

--Change this (important function) to take a previous vector, restore last face elements,
--apply all moves, discard elements of previous layers and assign current depth to them
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
