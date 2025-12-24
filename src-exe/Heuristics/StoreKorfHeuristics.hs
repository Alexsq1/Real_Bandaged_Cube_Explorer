module StoreKorfHeuristics(genPDBs) where

import CalculateKorfHeuristics(stdVectors)

import Data.Word(Word8)
import qualified Data.Vector.Unboxed as V
import qualified Data.ByteString as BS

-- | Generates PDBs until a maximum depth
genPDBs :: Word8 -> IO()
genPDBs maxDepth = do
    BS.writeFile (root ++ "c.pdb") (cBS)
    BS.writeFile (root ++ "e1.pdb") (e1BS)
    BS.writeFile (root ++ "e2.pdb") (e2BS)
    where
        root = "src-exe/Heuristics/pdb/" ++ (show maxDepth) ++ "/" 
        (c,e1,e2) = stdVectors maxDepth
        cBS = BS.pack (V.toList c)
        e1BS = BS.pack (V.toList e1)
        e2BS = BS.pack (V.toList e2)