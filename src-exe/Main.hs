module Main where


--when cabal repl, used modules must be imported here
--import Cube
import Bandaged
import Moves
--import InputCube
import InputBandagedCube
--import Visualizator
--import ManimHsConversion
--import Search
import Heuristic
--import IndexHeuristics
import GenKorfHeuristics

--import Data.Maybe
--import Data.List
--import qualified Data.Set as S
import qualified Data.Vector.Unboxed as V

--import OneSolve
--import OneInput
--import OneGen

--import Data.Maybe
--This is the entry point of cabal run


main :: IO ()
main = do
    --putStrLn "ho"
    --OneSolve.oneSolve
    --OneInput.oneInput
    --OneGen.oneGen

    let md = 4
    let v = cornersVector newSolvedBandagedCube md
    putStrLn ("Vector of corners: " ++ show (((take 10) . V.toList . (V.filter (< 20))) v))
    putStrLn ("Vector of corners: " ++ show (((take 20) . V.toList . (V.filter (<= md))) v))
    

    let ve = edgesFstVector newSolvedBandagedCube md
    putStrLn ("Vector of edges: " ++ show  (((take 10) . V.toList . (V.filter (< 20)))  ve ))
    putStrLn ("Vector of edges: " ++ show (((take 20) . V.toList . (V.filter (<= md))) ve))
    
