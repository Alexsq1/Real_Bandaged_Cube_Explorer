{-# OPTIONS_GHC -fno-cse #-}
-- This pragma avoids sharing the edges vectors when creation at emtpyVec

module CalculateKorfHeuristics where
--module CalculateKorfHeuristics(stdVectors) where

--import Bandaged
import Cube(Cube(..))
import Moves(Turn(..), Face(..), applyMove, axisOfFace, possibleTurns)
--import Moves(applyMove, possibleTurns)
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
    --(_, cPrev, e1Prev, e2Prev) <- loadLastVector currDepth
    ((_, cPrevPrev, e1PrevPrev, e2PrevPrev), (_, cPrev, e1Prev, e2Prev)) <- loadLastVectors currDepth
    let c = cornersVector   currDepth (cPrevPrev , cPrev)
    let e1 = edgesFstVector currDepth (e1PrevPrev, e1Prev)
    let e2 = edgesSndVector currDepth (e2PrevPrev, e2Prev)
    return (currDepth, c, e1, e2)

-- | Reads the previous two vector by accessing files. If asked for 0, generates empty vector of corresponding length
loadLastVectors :: Word8 -> IO (HVector, HVector)
loadLastVectors n
    | n == 0 = pure ((0, emptyVec 88179840, emptyVec 42577920, emptyVec 42577920), 
                     (0, emptyVec 88179840, emptyVec 42577920, emptyVec 42577920))
    | n == 1 = do
                    v <- loadVectors 0
                    return (v, v)
    | otherwise = do
                    ppv <- loadVectors (n-2) 
                    pv <- loadVectors (n-1)
                    return (ppv, pv)

-- the file exists if called from genPDB -> genPDBFromScratch -> stdVectors
-- genPDB ensures that the pdbs are generated from 0 in increasing order

-- | Generates empty vectors (all values are 255) of a given length
emptyVec :: Int -> Vector8
emptyVec n = V.fromList (replicate n 255)
--if goes fast enough, use it. If not:
{- emptyVec n = runST $ do
    mv <- MV.replicate n 255
    V.unsafeFreeze mv -}

-- | Generate a pattern database of corners from a state to depth n
cornersVector :: Word8                                        -- ^ Current depth
                -> (Vector8, Vector8)                         -- ^ Previous vectors
                -> Vector8
cornersVector n (ppv, pv) = applyChangesMV pv ch
    where
        --ch = bfsStoreChanges cornersKey n [R .. ] newSolvedCube
        ch = nextLayer Corner n (ppv, pv)

-- | Generate a pattern database of the first 6 edges from a state to depth n
edgesFstVector :: Word8                                          -- ^ Current depth
                -> (Vector8, Vector8)                            -- ^ Previous vectors
                -> Vector8
--edgesFstVector n (ppv, pv) = applyChangesMV pv ch
edgesFstVector n (_, pv) = applyChangesMV pv ch
    where
        ch = bfsStoreChanges edgesKeyFst n [R .. ] newSolvedCube
        --ch = nextLayer Edge1 n (ppv, pv)
        -- todo: change bfs to nextLayer

-- | Generate a pattern database of the last 6 edges from a state to depth n
edgesSndVector :: Word8                                        -- ^ Current depth
                -> (Vector8, Vector8)                          -- ^ Previous vectors
                -> Vector8
--edgesSndVector n (ppv, pv) = applyChangesMV pv ch
edgesSndVector n (_, pv) = applyChangesMV pv ch
    where
        ch = bfsStoreChanges edgesKeySnd n [R .. ] newSolvedCube
        --ch = nextLayer Edge2 n (ppv, pv)

-- | Make the inmutable vector (with mutable operations) 
applyChangesMV :: Vector8                                       -- ^ Previous vector
                -> [(Int, Word8)]                               -- ^ Changes
                -> Vector8
applyChangesMV v changes = runST $ do
    --mv <- MV.replicate sizeV defaultDepth
    mv <- V.unsafeThaw v
    myUpdate mv changes
    V.unsafeFreeze mv

myUpdate :: MV.MVector s Word8 -> [(Int, Word8)] -> ST s ()
myUpdate v changes = forM_ changes $ (\(i, value) -> MV.write v i value)




data HPiece = Corner | Edge1 | Edge2 deriving (Eq, Show)

nextLayer :: HPiece                  -- ^ Type of piece that is being calculated
            -> Word8                 -- ^ Current depth
            -> (Vector8, Vector8)    -- ^ Previous two vector
            -> [(Int, Word8)]
nextLayer p 0 _ = [(properKey p newSolvedCube, 0)]
nextLayer p cd (ppv, pv) = map (\i -> (i , cd)) adjCubesNewI
    where
        indexPV = V.zip (V.fromList [0 .. V.length pv - 1]) pv
        indexLL = (V.toList . (V.map fst) . (V.filter (\t -> snd t == cd - 1))) indexPV
        indexCubes = map (recompose p) indexLL
        adjCubes = concat $ map adjacent indexCubes
        -- missing elimination duplicates?
        adjCubesI = map (properKey p) adjCubes
        --adjCubesNew = filter (\c -> access ppv c && access pv c) adjCubesI
        adjCubesNewI = filter (\i -> access ppv i && access pv i) adjCubesI

        --access :: Vector8 -> Cube -> Bool
        --access v c = v V.! (properKey p c) == 255

        access :: Vector8 -> Int -> Bool
        access v i = v V.! i == 255

properKey :: HPiece -> Cube -> Int
properKey Corner = cornersKey
properKey Edge1 = edgesKeyFst
properKey Edge2 = edgesKeySnd


{- 
    1: find indices of value (currD - 1)    (V.filter (\t -> snd t == cd-1) (V.zip (V.fromList[0..]) xs))
    falta map fst
    2: recompose cubes of that value (recompose)
    3: apply all (18) moves to those cubes (concat, map)
    4: filter to only those that are (== currD)
    5: calc their key (map)
    6: store in changes term (zip (repeat currDepth))
-}

-- | Calculate all the adjacent cubes
adjacent :: Cube -> [Cube]
adjacent c = map (applyMove c) possibleTurns

recompose :: HPiece -> Int -> Cube
recompose Corner n = keysToCube (n,0,23442432)
--recompose Edge1 n = keysToCube (0,n,23442432)
--recompose Edge2 n = keysToCube (0,0,n)
-- Careful: can give strange failures because of solved pieces and repeated ones
-- NOTE: recompose for edges is wrong
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
