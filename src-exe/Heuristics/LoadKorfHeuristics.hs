module LoadKorfHeuristics(loadVectors, lookupAll) where

import Cube(Cube(..))
import IndexHeuristics

import qualified Data.ByteString as BS
import Data.Word(Word8)
import qualified Data.Vector.Unboxed as V

-- | Piece: private type for making accesses to vectors comfortable
data Piece = Corner | Edge1 | Edge2 deriving(Show, Eq, Ord, Enum)

type Vector8 = V.Vector Word8
type HVector = (Word8, Vector8, Vector8, Vector8)

loadVectors :: Word8 -> IO (HVector)
loadVectors depth = do 
    v1 <- readPDB (fileName Corner depth)
    v2 <- readPDB (fileName Edge1 depth)
    v3 <- readPDB (fileName Edge2 depth)
    return (fromIntegral depth, v1, v2, v3)

-- | Accesses the pattern database and return the minimum number of moves for each piece set
lookupAll :: HVector -> Cube -> (Word8, Word8, Word8)
lookupAll (n,c,e1,e2) bc = (vAccess n c cornersKey bc, vAccess n e1 edgesKeyFst bc, vAccess n e2 edgesKeySnd bc)

vAccess :: Word8 -> Vector8 -> (Cube -> Int) -> Cube -> Word8
vAccess n v kIndex bc
    | vecAcc == 255 = n+1
    | otherwise = vecAcc
    where
        vecAcc = (V.!) v (kIndex bc)

fileName :: Piece -> Word8 -> String
fileName p d = 
    root ++ case p of
        Corner -> "c.pdb"
        Edge1 -> "e1.pdb"
        Edge2 -> "e2.pdb"
    where
        root = "pdb/" ++ (show d) ++ "/"

readPDB :: String -> IO (Vector8)
readPDB fname = do
   vBS <- BS.readFile fname
   return (V.fromList (BS.unpack vBS))
