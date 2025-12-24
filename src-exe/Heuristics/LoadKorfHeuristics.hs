module LoadKorfHeuristics(loadVectors, lookupAll) where

import Bandaged
import IndexHeuristics

import qualified Data.ByteString as BS
import Data.Word(Word8)
import qualified Data.Vector.Unboxed as V

-- | Piece: private type for making accesses to vectors comfortable
data Piece = Corner | Edge1 | Edge2 deriving(Show, Eq, Ord, Enum)

type Vector8 = V.Vector Word8
type HVector = (Vector8, Vector8, Vector8)

loadVectors :: Int -> IO (HVector)
loadVectors depth = do 
    v1 <- readPDB (fileName Corner depth)
    v2 <- readPDB (fileName Edge1 depth)
    v3 <- readPDB (fileName Edge2 depth)
    return (v1, v2, v3)

-- | Accesses the pattern database and return the minimum number of moves for each piece set
lookupAll :: HVector -> BandagedCube -> (Word8, Word8, Word8)
lookupAll (c,e1,e2) bc = (vAccess c cornersKey bc, vAccess e1 edgesKeyFst bc, vAccess e2 edgesKeySnd bc)

vAccess :: Vector8 -> (BandagedCube -> Int) -> BandagedCube -> Word8
vAccess v kIndex bc = (V.!) v (kIndex bc)

fileName :: Piece -> Int -> String
fileName p d = 
    root ++ case p of
        Corner -> "c.pdb"
        Edge1 -> "e1.pdb"
        Edge2 -> "e2.pdb"
    where
        root = "src-exe/Heuristics/pdb/" ++ (show d) ++ "/"

readPDB :: String -> IO (Vector8)
readPDB fname = do
   vBS <- BS.readFile fname
   return (V.fromList (BS.unpack vBS))
