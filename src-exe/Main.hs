module Main where

--import InputAndSolve
import GenKorfHeuristics(genPDBs)

main :: IO ()
main = do
    --InputAndSolve.inputAndSolve 1
    --genPDBs (1 :: Word8)
    genPDBs 1
