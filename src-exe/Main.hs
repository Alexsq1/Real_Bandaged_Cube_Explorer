module Main where


--when cabal repl, used modules must be imported here
--import Cube
--import Bandaged
--import Moves
--import InputCube
--import InputBandagedCube
----import Visualizator
----import ManimHsConversion
--import Search
----import Heuristic
--
--import Data.Maybe
--import qualified Data.Set as S

--import OneSolve
--import OneInput
import OneGen

--This is the entry point of cabal run


main :: IO ()
main = do
    --OneSolve.oneSolve
    --OneInput.oneInput
    OneGen.oneGen
