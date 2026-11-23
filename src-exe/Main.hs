module Main where

--import InputAndSolve
import StoreKorfHeuristics
--import OneSolve
--import HeuristicsProfile

-- debugging:
import qualified Data.Vector.Unboxed as V
import CubeCreator
import Cube
import Bandaged
import Moves
import CalculateKorfHeuristics
import KorfHeuristic
import Data.Maybe

main :: IO ()
main = do
    putStrLn "Wellcome to Real Bandaged Cube Explorer!"

    let d = 2
    genPDBs d
    --InputAndSolve.inputAndSolve d
