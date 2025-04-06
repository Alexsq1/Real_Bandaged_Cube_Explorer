module TestSolutions (testSolutions) where

import Test.QuickCheck

import Search
import Cube
import Moves
import Bandaged
import InputBandagedCube
import Data.Maybe


testSolutions :: IO()
testSolutions = do
    putStrLn "tests of solutions not yet"
--    quickCheck searchSolves
--    quickCheck optimalityScramble


searchSolves :: Algorithm -> Property
searchSolves a = solved (algToPerm (Algorithm (xs1 ++ xs2))) === True
    where
        origin = newBandagedCube (newCubeFromList [0..53]) [[]]
        scr = fromJust (tryToExecuteAlg origin a)
        solve = fromJust (genericSearch scr solvedBC allPossibleMoves (const 0))
        Algorithm xs1 = a
        Algorithm xs2 = solve

optimalityScramble :: Algorithm -> Bool
optimalityScramble a = length xs2 <= length xs1
    where
        origin = newBandagedCube (newCubeFromList [0..53]) [[]]
        scr = fromJust (tryToExecuteAlg origin a)
        solve = fromJust (genericSearch scr solvedBC allPossibleMoves (const 0))
        Algorithm xs1 = a
        Algorithm xs2 = solve