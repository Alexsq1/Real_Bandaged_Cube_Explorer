module TestSolutions (testSolutions) where

import Test.QuickCheck

import Search
import Cube
import Moves
import Bandaged
import InputBandagedCube
import Data.Maybe
import KorfHeuristic
import MoveGeneration

testSolutions :: IO()
testSolutions = do
    putStrLn "tests of solutions"
    quickCheck genericSearchSolvesOptimally
    quickCheck optimalityScramble

genericSearchSolvesOptimally :: Algorithm -> Property
genericSearchSolvesOptimally a = (length xs1 < 10) ==>
    (solved (algToPerm (Algorithm (xs1 ++ xs2))) === True)
    where
        origin = newBandagedCube (newCubeFromList [0..53]) [[]]
        scr = fromJust (tryToExecuteAlg origin a)
        solve = fromJust (genericSearch scr solvedBC sixAxis (korfHeuristic))
        Algorithm xs1 = a
        Algorithm xs2 = solve

optimalityScramble :: Algorithm -> Property
optimalityScramble a = (length xs1 <= 9) ==> length xs2 <= length xs1
    where
        origin = newBandagedCube (newCubeFromList [0..53]) [[]]
        scr = fromJust (tryToExecuteAlg origin a)
        solve = fromJust (genericSearch scr solvedBC sixAxis (korfHeuristic))
        Algorithm xs1 = a
        Algorithm xs2 = solve


