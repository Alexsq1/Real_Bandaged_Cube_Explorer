module LoadKorfHeuristics(loadVectors, lookupAll, HVector(..)) where

import qualified Data.ByteString as BS
import Data.Word(Word8)

import Bandaged
import IndexHeuristics

import qualified Data.Vector.Unboxed as V

{-There should be a function to load the pdb in a file in this module-}

--import Debug.Trace (trace, traceShow)

-- | Alias for Word8 Vectors
type Vector8 = V.Vector Word8
type HVector = (Vector8, Vector8, Vector8)

-- | Accesses the pattern database and return the minimum number of moves for each piece set
loadVectors :: Int -> IO (Vector8, Vector8, Vector8)
loadVectors depth = do 
    v1 <- readPDB (fileName 0 depth)
    v2 <- readPDB (fileName 1 depth)
    v3 <- readPDB (fileName 2 depth)
    return (v1, v2, v3)

lookupAll :: HVector -> BandagedCube -> (Word8, Word8, Word8)
lookupAll (c,e1,e2) bc = (vAccess c cornersKey bc, vAccess e1 edgesKeyFst bc, vAccess e2 edgesKeySnd bc)

vAccess :: Vector8 -> (BandagedCube -> Int) -> BandagedCube -> Word8
vAccess (v) kIndex bc = (V.!) v (kIndex bc)

fileName :: Int -> Int -> String
fileName d maxDepth = 
    root ++ case d of
        0 -> "c.pdb"
        1 -> "e1.pdb"
        2 -> "e2.pdb"
    where
        root = "src-exe/Heuristics/pdb/" ++ (show maxDepth) ++ "/"

readPDB :: String -> IO (Vector8)
readPDB fname = do
   vBS <- BS.readFile fname
   return (V.fromList (BS.unpack vBS))
