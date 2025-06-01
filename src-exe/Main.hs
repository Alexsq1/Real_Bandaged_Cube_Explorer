module Main where

import InputAndSolve
--import OneInput
--import OneSolve
import HundredSolves

import Moves
import Cube
import Bandaged
import InputCube
import InputBandagedCube
import SolvingStrategies

main :: IO ()
main = do
    InputAndSolve.inputAndSolve
    --OneSolve.oneSolve
    --HundredSolves.hundredSolves