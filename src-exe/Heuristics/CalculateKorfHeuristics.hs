{-# OPTIONS_GHC -fno-cse #-}
-- This pragma avoids sharing the edges vectors when creation at emtpyVec

--module CalculateKorfHeuristics(stdVectors) where
module CalculateKorfHeuristics where

import Cube(Cube(..))
--import Moves(Turn(..), Face(..), applyMove, axisOfFace, possibleTurns)
import Moves(applyMove, possibleTurns)
import CubeCreator(newSolvedCube)
import IndexHeuristics
import LoadKorfHeuristics(loadVectors)

--from recompose: (maybe move to IndexHeuristics)
import MathematicalNotation(mathToCube)
import Combinatorics(nprDecode, decodeEO)

--import qualified Data.Set as S
--import Data.Maybe(fromJust)
import Data.Word(Word8)
import Data.List((\\), nub)

--import Data.PSQueue as PS
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
edgesFstVector n (ppv, pv) = applyChangesMV pv ch
    where
        --ch = bfsStoreChanges edgesKeyFst n [R .. ] newSolvedCube
        ch = nextLayer Edge1 n (ppv, pv)

-- | Generate a pattern database of the last 6 edges from a state to depth n
edgesSndVector :: Word8                                        -- ^ Current depth
                -> (Vector8, Vector8)                          -- ^ Previous vectors
                -> Vector8
--edgesSndVector n (ppv, pv) = applyChangesMV pv ch
edgesSndVector n (ppv, pv) = applyChangesMV pv ch
    where
        --ch = bfsStoreChanges edgesKeySnd n [R .. ] newSolvedCube
        ch = nextLayer Edge2 n (ppv, pv)

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
        -- zip the numbers with their indices
        indexPV = V.zip (V.fromList [0 .. V.length pv - 1]) pv

        -- select indices of the last layer
        indexLL = (V.toList . (V.map fst) . (V.filter (\t -> snd t == cd - 1))) indexPV

        -- recompose the cubes
        indexCubes = map (recompose p) indexLL

        -- calculate the adjacent cubes of the corresponding
        adjCubes = concatMap adjacent indexCubes

        -- calculate the key of the (potentially) new layer, remove duplicates
        adjCubesI = nub (map (properKey p) adjCubes)

        -- remove the previously visited states
        adjCubesNewI = filter (\i -> access ppv i && access pv i) adjCubesI

        access :: Vector8 -> Int -> Bool
        access v i = v V.! i == 255

properKey :: HPiece -> Cube -> Int
properKey Corner = cornersKey
properKey Edge1 = edgesKeyFst
properKey Edge2 = edgesKeySnd

-- | Calculate all the adjacent cubes
adjacent :: Cube -> [Cube]
adjacent c = map (applyMove c) possibleTurns

recompose :: HPiece -> Int -> Cube

-- recompose for corners: fill with solved edges
recompose Corner n = keysToCube (n,0,23442432)

-- recompose for edges: fill with solved corners and the remaining edges
recompose p ek = mathToCube ([0..7], replicate 8 0) (ep, eo)
    where
        --Separate permutation and orientation of each piece type
       (epk, eok) = quotRem ek (2 ^ (6 :: Int)) 

       --Decode perm and orientation halves (each list of 6 elements)
       epHalf = nprDecode epk
       eoHalf = decodeEO eok

       (epFull, eoFull) = complete p (epHalf, eoHalf)

       --Reorder the edges
       (ep, eo) = reorderEdges (epFull, eoFull)

       sixZeros = replicate 6 0

       -- adds solved pieces at the begginning/end depending on the piece
       complete :: HPiece -> ([Int], [Int]) -> ([Int], [Int]) 
       complete Edge1 (ep', eo') = (ep' ++ ([0..11] \\ ep') , eo' ++ sixZeros)
       complete Edge2 (ep', eo') = (([0..11] \\ ep') ++ ep' , sixZeros ++ eo')
       complete _ _ = ([], [])

{- Future: not recomposing the whole cube, only specific pieces (efficiency). 
 - Not sure if lazy evaluation do that -}

