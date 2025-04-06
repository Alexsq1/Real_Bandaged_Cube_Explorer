module Main where

import TestCube
import TestMoves
import TestSolutions

--main-is of cabal test
main :: IO()
main = do
    putStrLn "\n\n\n"
    putStrLn "Testing Cube:"
    testsCube

    putStrLn "\n"
    putStrLn "Testing Moves:"
    testsMoves
    --Test of moves is not working

    putStrLn "\n"
    putStrLn "Testing Solutions:"
    testSolutions


    --Add here the tests of new modules

