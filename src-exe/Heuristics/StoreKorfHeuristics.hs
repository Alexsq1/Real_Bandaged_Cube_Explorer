module StoreKorfHeuristics(genPDBs) where

import CalculateKorfHeuristics(stdVectors)

import Data.Word(Word8)
import qualified Data.Vector.Unboxed as V
import qualified Data.ByteString as BS
import System.Directory(doesFileExist, getFileSize, createDirectoryIfMissing)

-- | Generates PDBs if it does not exists previously. If it existed, nothing happens
genPDBs :: Word8 -> IO()
genPDBs 255 = pure ()
genPDBs maxDepth = do
    --generate previous pdb (n-1)
    genPDBs (maxDepth - 1)

    -- if files do not exist, create them
    exists1 <- doesFileExist (cornersFile)
    exists2 <- doesFileExist (edge1File)
    exists3 <- doesFileExist (edge2File)
    if (exists1 && exists2 && exists3) 
        then pure () 
        else (
            do
                createDirectoryIfMissing True root
                writeFile cornersFile ""
                writeFile edge1File ""
                writeFile edge2File ""
        )

    -- if the files are empty, generate them
    t1 <- getFileSize cornersFile
    t2 <- getFileSize edge1File
    t3 <- getFileSize edge2File 

    if (t1 + t2 + t3 == 0)
        then(
               do 
               putStrLn ("Pattern databases must be generated ONLY for the first time. Generating at depth "++ 
                   (show maxDepth) ++ ". This process can take some minutes. Please, be patient.")
               genPDBsFromScratch maxDepth
            ) else pure ()
    where
        root = "src-exe/Heuristics/pdb/" ++ (show maxDepth) ++ "/" 
        cornersFile = root ++ "c.pdb"
        edge1File = root ++ "e1.pdb"
        edge2File = root ++ "e2.pdb"

-- | Generates PDBs from scratch until a maximum depth
genPDBsFromScratch :: Word8 -> IO()
genPDBsFromScratch maxDepth = do
    (_,c,e1,e2) <- stdVectors maxDepth
    let cBS = BS.pack (V.toList c)
    let e1BS = BS.pack (V.toList e1)
    let e2BS = BS.pack (V.toList e2)
    BS.writeFile (root ++ "c.pdb") (cBS)
    BS.writeFile (root ++ "e1.pdb") (e1BS)
    BS.writeFile (root ++ "e2.pdb") (e2BS)
    where
        root = "src-exe/Heuristics/pdb/" ++ (show maxDepth) ++ "/" 

