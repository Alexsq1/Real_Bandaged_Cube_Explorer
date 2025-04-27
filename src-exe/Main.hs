module Main where


--when cabal repl, used modules must be imported here
--import Cube
--import Bandaged
--import Moves
--import InputCube
--import InputBandagedCube
--import Visualizator
--import ManimHsConversion
--import Search
--import Heuristic
--import IndexHeuristics
--import GenKorfHeuristics
--import MoveGeneration
--import SolvingStrategies
--import Data.Maybe
--import Data.List
--
--import qualified Data.Set as S
--import qualified Data.Vector.Unboxed as V
--import Data.Maybe
--import Data.List

import OneSolve
import OneInput
--import OneGen

--This is the entry point of cabal run


main :: IO ()
main = do
    OneSolve.oneSolve
    OneInput.oneInput
    --OneGen.oneGen
