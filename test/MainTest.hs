module Main where

import TestCube
import TestMoves
import TestSolutions
import TestHeuristics

--main-is of cabal test
main :: IO()
main = do
    --putStrLn "\n\n\n"
    --putStrLn "Testing Cube:"
    --testsCube
--
    --putStrLn "\n"
    --putStrLn "Testing Moves:"
    --testsMoves
    ----Test of moves is not working
--
    ----putStrLn "\n"
    ----putStrLn "Testing Heuristics:"
    ----testHeuristics
    --putStrLn "Removed testing of heuristics"

    putStrLn "\n"
    putStrLn "Testing Solutions:"
    testSolutions

    putStrLn "\n"


    --Add here the tests of new modules



{-
REMAINING TESTING 

BANDAGED
tryToExecuteAlg and unsafe: relation

InputBandagedCube
Expanded block always have 1 edge
Canonic sets, when adding same element to all, returns single set
If all are disjoing, returns the same
fromManim and toManim are inverses (almost, with adaptation)

-}