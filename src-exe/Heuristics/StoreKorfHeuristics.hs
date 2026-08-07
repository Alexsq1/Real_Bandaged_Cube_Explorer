module StoreKorfHeuristics(genPDBs) where

import CalculateKorfHeuristics(stdVectors)

import Data.Word(Word8)
import qualified Data.Vector.Unboxed as V
import qualified Data.ByteString as BS

-- | Generates PDBs until a maximum depth
genPDBs :: Word8 -> IO()
genPDBs maxDepth = do
    (_,c,e1,e2) <- stdVectors maxDepth
    let cBS = BS.pack (V.toList c)
    let e1BS = BS.pack (V.toList e1)
    let e2BS = BS.pack (V.toList e2)
    BS.writeFile (root ++ "c.pdb") (cBS)
    BS.writeFile (root ++ "e1.pdb") (e1BS)
    BS.writeFile (root ++ "e2.pdb") (e2BS)
    where
        root = "pdb/" ++ (show maxDepth) ++ "/" 