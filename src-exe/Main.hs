module Main where


--when cabal repl, used modules must be imported here
import Cube
import Bandaged
import Moves
import InputCube
import InputBandagedCube
--import Visualizator
--import ManimHsConversion
import Search
import Heuristic
import IndexHeuristics
import GenKorfHeuristics

import Data.Maybe
--import Data.List
--import qualified Data.Set as S
import qualified Data.Vector.Unboxed as V

--import OneSolve
--import OneInput
import OneGen

--import Data.Maybe
--This is the entry point of cabal run


main :: IO ()
main = do
    --putStrLn "ho"
    --OneSolve.oneSolve
    --OneInput.oneInput
    OneGen.oneGen

    --let v = cornersVector newSolvedBandagedCube
    --putStrLn ("Vector of corners: " ++ show (V.filter (<20) v))
--
    --let ve = edgesFstVector newSolvedBandagedCube
    --putStrLn ("Vector of edges: " ++ show (V.filter (<20) ve))
    
