module HundredSolves where

--import Test.QuickCheck

import Cube
import Moves
import Bandaged
import InputBandagedCube
import Data.Maybe
import Data.List (group, sort)
import SolvingStrategies(smartKorfSolver)
--import MoveGeneration(freeFaces)



hundredSolves :: IO ()
--Solve 2-gen
hundredSolves = solve [[49,51,52,53]] [] scrambles2Gen

--hundredSolves = solve [[49,51,53]] [] scramblesRUL



solve :: [[Int]] -> [Int] -> [Algorithm] -> IO()
solve _ acc [] = do
    putStrLn (show (count acc))
solve blocks acc (scramble:xs) = do
    let origin = newBandagedCube (newCubeFromList [0..53]) blocks
    let scrambeledCube = fromJust (tryToExecuteAlg origin scramble)
    let solution = fromJust (smartKorfSolver scrambeledCube)
    putStrLn ("\nSolved with solution: " ++ (show solution))
    solve blocks (lengthAlg solution : acc) xs


count :: [Int] -> [(Int, Int)]
count xs = map (\g -> (head g, length g)) . group . sort $ xs


scrambles2Gen :: [Algorithm]
scrambles2Gen = map (\t -> read t :: Algorithm)
    [""
    , "R' U' R U2 R2 U R U2 R' U2 R' U' R2 U R2 U2 R U2" 
    , "U2 R' U2 R U2 R' U'" 
    , "R' U' R U2 R2 U R U R U2 R U2 R' U' R' U" 
    , "U2 R2 U' R2 U2 R U' R2 U2 R'" 
    , "R U2 R' U R2 U' R U2 R2 U R2 U' R' U2 R2 U' R2" 
    , "U' R2 U' R U2 R' U R' U2 R U R2 U' R' U'" 
    , "U R U2 R2 U R U R2 U R U R U' R2 U2 R'" 
    , "R U'" 
    , "U' R2 U R U2 R U2 R' U R U2 R U R" 
    , "U R U' R' U R' U R U2 R2" 
    , "R2 U' R' U R' U2 R'" 
    , "U2 R2 U R U' R U R' U R' U" 
    , "U' R U2 R2" 
    , "R2 U2 R' U' R U2 R' U2 R2 U' R2 U R2 U R2 U2" 
    , "R U R U R2 U R' U' R2 U2 R2 U R2 U' R" 
    , "R2 U R2 U' R2 U' R U' R U R' U'" 
    , "R U2 R2 U R' U' R U2 R U R" 
    , "R2 U2 R' U R2 U2 R' U2 R U2 R2 U' R' U' R" 
    , "U2 R U2 R U R U R'" 
    , "R' U' R U' R2 U" 
    , "R U2 R U R' U' R U2" 
    , "U' R U R U R2 U2 R U" 
    , "U' R2' U' R U2 R U' Solution: R' U2 R' U2 R2 U' R' U' R U2 R U' R2 U' R2" 
    , "R U2 R' U R2 U' R2 U2 R U2 R2 U' R' U2" 
    , "R U2 R U R2 U R' U R2 U' R' U2 R'" 
    , "R U' R2 U'" 
    , "R U R' U' R U' R U R U R U2 R2 U' R U R2" 
    , "U2 R" 
    , "U" 
    , "R2" 
    , "R U R2 U' R2 U' R U R U' R U' R2" 
    , "U2 R' U' R2 U R2" 
    , "R' U R2 U2 R' U R' U' R' U R U2 R U R U2" 
    , "R2 U R U2 R2 U R U' R U2 R2 U2 R U2 R' U" 
    , "U2 R' U2 R' U' R2 U2 R' U2 R' U' R' U' R U2" 
    , "U R U' R' U' R U' R U' R' U2 R U2 R' U" 
    , "U R2 U'" 
    , "R' U R' U' R' U2 R2 U2 R2 U R2 U' R U' R' U2" 
    , "U2 R2 U' R U R U2 R U' R' U' R2 U R" 
    , "U' R' U R" 
    , "R U R U2 R U2 R U2 R' U2" 
    , "U R U2 R U R2 U R U2 R U' R2 U' R2" 
    , "R U2 R' U R2 U' R' U2 R U R' U2 R' U R U" 
    , "R' U R2 U2 R' U2 R' U' R' U2 R' U" 
    , "U' R' U2 R2 U2 R' U R U R2 U2 R' U'" 
    , "U2" 
    , "R U' R2 U' R' U2 R U" 
    , "U' R2 U' R' U2 R U' R U' R' U R2" 
    , "U R U R U R U2 R U2 R U2 R' U2 R2" 
    , "U R U2 R U' R' U2 R2 U2 R' U' R U' R U2 R2 U' R2" 
    , "R U2 R2 U' R2 U R' U2 R' U R2 U R U' R U2" 
    , "U R'" 
    , "U R U" 
    , "R2 U2 R' U2 R U R' U2 R2 U' R2 U R' U R" 
    , "R' U' R U' R U' R2 U R U' R U2 R' U2 R'" 
    , "R2 U2 R U2 R2 U2 R' U R'" 
    , "R2 U2 R' U2 R' U' R U' R2 U' R U2 R2 U R2 U2" 
    , "U' R2 U2 R2 U R U' R' U2" 
    , "R2 U' R U2 R2 U R2 U R'" 
    , "R'" 
    , "U' R' U' R' U' R2 U2 R U' R U2 R2 U" 
    , "U R'" 
    , "U' R2 U' R U R2 U2 R2 U2 R2 U R2 U R'" 
    , "R'" 
    , "R' U' R2 U2 R2 U' R'" 
    , "R' U2 R' U'" 
    , "U R U R U2 R2" 
    , "R U R U2 R2 U' R2 U R' U" 
    , "R2 U' R U R2" 
    , "U2 R2 U R2" 
    , "U' R2 U' R U R U' R' U2 R2 U2 R U2 R" 
    , "R2 U R' U R2 U2 R' U' R' U' R U2 R2 U R2 U'" 
    , "U' R2 U2 R' U2 R U R U R" 
    , "U' R2 U R' U2 R U2 R U' R2 U2 R'" 
    , "U' R U2 R' U' R2 U' R2 U' R' U' R2 U' R2 U2" 
    , "U2 R2 U' R' U R' U' R' U' R' U R'" 
    , "U' R2 U' R U' R' U' R U2 R' U2" 
    , "R2 U' R' U2 R2 U2 R U R2 U' R U'" 
    , "R U' R U2 R U R' U' R2 U R U' R' U R2 U'" 
    , "R' U' R U' R2" 
    , "U' R U R U R' U R2 U' R2 U2 R U R2 U" 
    , "R U2 R U R U' R' U R2 U R U R' U" 
    , "R U2 R U2 R2 U' R' U R2 U' R U' R' U R U'" 
    , "R' U R U' R' U' R2 U2 R2 U R' U2 R' U2" 
    , "R2 U2 R2 U R U R' U' R2 U' R U' R2 U2 R2 U R" 
    , "R U' R' U' R U2 R' U' R' U2 R U R' U R2 U2 R2" 
    , "R' U" 
    , "U2 R' U R" 
    , "R2" 
    , "R2 U R U' R' U2 R U2 R' U2 R U R' U2 R U2 R'" 
    , "U R" 
    , "U R2 U R2 U R2 U R" 
    , "R U R U R U R' U2 R2 U R' U2 R' "
    ]

scramblesRUL :: [Algorithm]
scramblesRUL = map (\t -> read t :: Algorithm)

    [   "",
    "L2 L' R2 L2 R L2 L' L2 R2 U' R' R U R U2 U L' U' L R' U L R' L' L2 R2 L R R' U2 R L' R R R2 L U' R L U' L' U' U2",
    "R2 U R U2 U' L R' R2 L' R R' R R' U L R L U L2 R' L2 U2 L L' R R2 U R L U2 R U' R2 U2 R R U2 R2 L U2 L U L L' R' L' U' L2 B2",
    "L R L2 L2 R R U' U L' L2 U L' U L2 R U' R2 U R L R U2 R' L' U2 L R2 U' L' R2 U' L R2 U L L2 L R2 L2 R2 L' U' L2 U' L R2 U' U R2 U2 R",
    "L L R' R' L2 R2 U2 L2 L2 R2 R U L' L2 R2 U' L' R' U' R2 R U' L U2 R U' R2 L' U2 L R' U2 L' U' R U' R U2 R2 U' L2 U' L2 R2 U2 U' R' U2 R U2 L",
    "L R L2 L2 U L' R U' L2 R U2 L2 R2 L R L2 R2 L R L' U2 R L' R L U2 L2 R U R2 U R2 L' U L2 R U2 L' R2 U L R L2 R L R L' U L' L R R' R' L2 R F'",
    "U2 L' R2 U L R' R U' R2 U R2 L' R2 U L2 U R' U2 L R' U L R2 R L2 U2 L' R2 L R' U U L' R U2 L' U2 R' U2 L' R' U U L2 L R L' R2 U U' R2 R' U2 R' L2 R' U' R2 L R U' L' F2",
    "L' U' L2 U2 L2 U2 R U2 R2 U' R' L2 R U' U U' L' R U U L' R' R L R L R' R2 L L2 R' L R2 L' U2 R' U L2 R U2 L U L2 R2 U2",
    "L' R L L U U L' U L' L2 R2 L2 R' U' R2 U L2 R' L2 R2 U L U L R U R L' R2 R' R' U L' R L R U' L' R2 U2 U' L L' R L2 R' U2 L2 R' F2",
    "U U L R U2 L R' U' L R U' R R' L2 U2 R2 U' L' R2 U L2 R L R2 U U' L R' R2 U' U' R U' R' U L R' U' L2 R L' R U L' R L' R2 U' U L' R L F'",
    "L' R2 U L' R2 L2 R' L R' U2 L2 L R U2 L L R2 U L R2 L2 L2 U' L U' L R L2 U U' L2 U L' R U U L2 R2 R' U' L R U2 L2 R' L L2 U2 L' U2 U' F'",
    "L' U U' L R L2 R' R U2 L2 R2 U2 L R2 U2 U L' R L' U2 U2 L' R' R' U2 R2 L R2 U L R' U' L2 U2 L R R2 L' R U L2 R' L2 R L R U2 L' R U",
    "L2 L' R2 L' L2 R2 U' L R2 U U2 R' L' R' R' L' R R' L2 U' R2 L R2 U2 U' U L2 R2 L' R U L' R2 L2 R2 R U L' R U2 L2 R L2 U U' L2 R2 R' L2 R U L'",
    "R U' L' U L' U2 L2 R L' L L' R' U2 L2 R U R U L' R U' R U' U U' L' R U' L' R L' R' L2 R2 L R U' L2 U' R U R2 U' R R R U R2 F",
    "L2 L' R' U L' R2 U2 L2 L' L U' L' R2 U2 R2 U2 L' U2 L R U2 L R2 L2 U' R' U' L' R U L' U' L L R U' L2 R2 U U L2 R2 L2 R' U2 R2 L' R' R' L F",
    "R2 L' U U2 L2 R L R2 R U2 L R2 U2 L2 R2 U' L U2 R U U' L' R' R' L R2 U' R' U L' R' U U R2 U2 U U L2 R' U2 R' U' R' U2 L2 U2 R L2 R' U2 U2 R' U U L' U2 U",
    "U2 L2 R2 R' U2 L' R' U2 U' R L' L' U U L R U' L' R2 L R2 L L' R' U2 L R' U' L L' R2 U' L' R2 U2 U L2 L R2 U' L' R2 R' U2 R' L2 U R2 U2 R' U2 L' R' U2 R'",
    "U' U' L2 R2 L R2 U L' L' R2 U' L' U R U2 L' R U R U' L R2 L2 R U L' L2 R L' R' L' L2 R2 L' L2 R L' R U' L2 R U U L2 R' F",
    "U U2 L' R2 U' L2 L U R' R' U' L' R' R U' R U L' R2 U2 L' R R2 U L2 U2 L R' L' R L R' U2 R' R' L R2 R U2 R' L' R2 U' L2 U2 U' R2 U U2 B2",
    "L2 R' U R' U' R2 U' R U' L' R2 L' R L' R U2 L' U2 L R2 U' L' R2 U' L' R2 U2 L' L2 R R' U' U2 U L R' L' R L' U L U U R2 R2 U U' L' U U2 R L L2 U2",
    "U R2 U' L2 R R U' U' L R2 U L L2 R' L L R' U' L R L R R2 U2 L2 U2 L' R2 U' L' R' R L R U L' R2 U2 L U L' R2 U2 L R2 L R U2 L R U' R' L R U2",
    "U2 U' U R U' U2 R U' L' R2 L' U2 U2 R2 U' R U2 L R2 U' U' L' R L2 L2 R U L' R U R2 U L' R' U' R' U2 L' R' R L2 U L2 R U2 R' L U L R L",
    "U L' R' L2 R' L L R U' L R' L U L R2 L R2 L2 R U L' L2 R' U' U R U' L2 U' L' L R R U2 R U' R2 U2 R2 U2 L2 R2 R' U L2 U2 L' R'",
    "U' L' L R' L' L2 L R' R' L R' L' U2 U' R' U L U L2 U2 R' L U L U' L' R U' L' L2 R R2 U' L R L2 R2 R L2 R U2 R2 U L' R2 L2 R2 U' R'",
    "L2 R U2 R U' R2 L U U' L' U' U U' R' L R U' L U R L' L' R R U R2 U L' L' R2 L' R U' U' L' R2 L L U L R R2 U' L2 R' L U' L2 U L U' R'",
    "L2 L L' U2 U2 R' U' R2 U' L2 U L2 R L2 R' L2 U2 L' U' U L R2 U R U' L U L' U2 R2 L U L' R' U R2 L2 R2 U R U' L R U L R2 L2 U2 L2 R2 L R' R' U2 F",
    "L R U L L' U R' U L' L L R U' L U2 L R' U' L2 U' R U' R2 L L' L' L' R2 L' L' R2 U2 U' U2 L U U R2 U2 L' R L L' R2 B'",
    "U L U2 R' R' L' R' R2 U L U2 R U' L' R U2 L2 U2 R R' U' L' U2 R L2 R R L2 U L2 R R' U L U' L' U' L R' L R2 U L' L U' R R2 U L' R2 U2 L'",
    "U2 L' U' R U' L' R2 U' R2 U' L2 R L2 R2 U2 U' U L R' L2 R2 U R U' R2 R U' U' R2 L' R' U2 U' L' L2 R U R' U L' R2 U' U' L2 R2 U2 U2 L' L' R L U2 F",
    "U U L' U2 L' R' L' U' L2 R' U2 L2 R R2 U2 U2 L2 R U L2 U U' L2 R' U2 U' R' R L' R L2 U2 R' R U2 U2 L R' U' R2 U2 R U2 R' U2 L2 R' L2 R U' L U L U2 R L' R",
    "L2 R R' U' U2 L' R2 U L' R U2 L L R' U L L' L' R U2 L' R' U L U2 L2 U2 R' U L L' R' U2 U' U R U2 R2 U' L2 R2 U' L' R U2 L' R R U R",
    "R L2 R2 L2 R U2 L U U2 U U' L U L2 R' U2 L2 U U U' R' L' L' R2 U2 U' U2 R' U2 L2 L R R2 U' U L' R' U2 L2 L R' L L'",
    "L R' U2 L' R' U2 L2 R U' U L' R' U L R' U' U' U U R' U L' U2 U2 L2 R R U2 L2 R' U' U' R' R2 R R U' L2 U",
    "U R' U2 L' R' U2 L' R2 U R L' U' U L' R' U' U' L U' L' R2 U2 L L2 R2 L2 U2 L' R U2 U2 U2 L R L' R L' R L R U L' U' U' L'",
    "L' L2 R' L' R2 R2 U2 R L2 U R2 U2 U' R' U' L U2 L' U L2 R U' L' L2 U' R2 U L2 U' L2 R2 L2 U R U L2 U2 R2 L2 U2 L2 R' U' R' U2 L R2 U2 F",
    "L' L R' U' R2 U' U L2 R2 R L U2 L2 R' L2 R L2 R2 R2 U' L' L2 L' L U' R U U' R U2 U' R U' L' R L L' U2 U2 L R' U' U2 U' L2 R L2",
    "U2 L' R L' R' U' L U2 R U L2 U U R' L U L' L' R' U2 L R' L' L2 R2 U2 U2 L2 U2 L' L R' L2 L' R2 L' L' R' L' R' U L L L' U U2 R U2 L2 R R",
    "R2 L' R' L R2 U' R' U2 U' R U R' U R U' R2 U2 L' U' R L R2 U2 L2 U L R2 U2 L2 U U' U' R2 U' L U2 L' U' L L U2 L2 U2 R R' R' U' L2 L'",
    "U' L2 U2 L U R2 L U2 R' L' U' L2 L' U R' U2 L2 R' U2 L2 L2 U2 R' U2 L R2 L' L2 R U' L' U R' L' R L R2 U U' R L R R2 U L2 U L' R2 U L R U2 L R",
    "U' L' L2 L' R2 U2 U2 U' L R' U' L' R2 U' R L' L' R' U U' R' L R U R' R' L L2 R' R' U L L R' L2 R' L2 L L' R' L' R' U L L2 R'",
    "L2 L' L R2 U L L' R L' R L R' L2 R' L2 U R2 L' L2 R2 U R2 U' L R2 U2 L R2 L' R R2 U2 U2 U R' R2 U' R R L2 R L L2 B",
    "R' R' L R L2 L U' L' R2 R' L' R U' U2 L2 R' U U2 R' U' R2 L R' R2 L2 R' U2 L R2 U2 L R2 U' L R U L U2 R' U'",
    "R2 U2 R U' L R2 L2 L2 R L R U' R2 U' R' U2 L U' R R' U2 R R2 L R L R' L2 R R2 U L' R U' U2 L2 R' U' R2 L R U' L' U2 U' U L'",
    "U2 L2 L' L' L R L U' R2 U' R2 U2 R U' L2 U2 U2 R U L' U' R2 U2 L' R' L' R' U R L2 U2 R L2 U' L' R2 L R' L' L R L R U F2",
    "U2 L' R' U2 R2 U2 L2 R' L2 R L2 R2 L2 R' U' L2 R' L2 R R' U2 L R U U2 L2 R L' R' L R L R L R' U R' U R R' R2 U' L R2 R' U' L' R F2",
    "U L L U' L R2 U' R2 U U' R L L' R2 L2 U2 L L' R' L R' U L L2 U2 L' U L R' L R L' R' L U' L' U' L2 R' U2 L R U' R2 U2 L R2 U L' L' R2 L2",
    "L2 U' L' U' L2 U2 L' L2 U2 L U2 L2 R' U2 L2 U' R2 U2 L' R2 U' L R U' L' L' R2 U' L L L R U R U' L' L' R' U2 L' R U' U' R L U2 R U' L2 R L2",
    "U L L2 R2 U2 L' U2 R U' L R L2 L U' R' L U' L R' U' L2 R2 U2 L2 R2 U' L R' L' R2 R2 L R2 U L' R' U2 R2 U' L' R U2 L R U2 L R' U' F2",
    "R2 R R U L2 R' L2 L' U2 R' L2 R U2 L2 U' R2 L' R' R' U' L U' R U L R' U' R' R2 U2 R2 U2 L U L R U2 R2 L R' L2 R2 L2 R2 U' L2 L2 U2 U' B2",
    "U' L2 U' R U' R U' L2 U' L R R' U R' U2 R L R' U2 R U' L R U2 R2 U' L' R2 R L' R' L' R' R2 L R U2 L' R2 U L U L' R' L R L2 R R2 R L2 R U2 U'",
    "U' L' R2 U' U U' L L2 R' R' U' L R2 U' R2 U U' U U' R' U' L L' R' L L2 R' U L' L' R' L R' R U' L R2 L R' U L2 R2 R' U' R2 L2 U2 L2 R U L2 R' F'",
    "L' R' L R U2 R2 U L L' R2 R' U L L' R R' U' R R2 U R U R2 L2 R U R U' L R2 U2 R2 U R' U L U' R' U2 L L' L' D",
    "R R L U L' U2 R2 U' L' R U' R2 U L' R2 R U L' R2 L2 R2 L' R2 U L2 L2 L2 R2 U R L R U L2 R2 L' R' L' R U2 L U' L2 U' L L L R' R2 U' L' R2 F'",
    "L' U2 L R' U2 L' L' R U2 R' U L2 R' L' R2 U' L L' U2 L' L' R L' L2 R' L2 R2 L R2 U L R2 L2 R2 U L2 R' L' U L2 R U' L2 R R' L R U R2 L R' L R2 U' L2 R U' R'",
    "U2 U L' L2 U2 L R L' U' L' R R2 U L' R' U2 L2 R' L L2 R R2 R2 U' L' R L' R' L L2 U U' L R' U2 L2 R2 U2 L2 R' R U2 L2 R L2 R2 U2 L2 R' U' L2 U' R2 D",
    "U' L U' R L R2 L2 U2 U2 R2 U' L R2 U2 L L R' U' U R' U R L R L U' L' R' U U2 R2 L R R2 U' L R' U' L R L R2 U2 L2 R2 L2 D2",
    "U2 R' L' U' R2 U' R2 L' L2 L U2 R2 U2 R L' U L R2 U U U' U2 L' U' L' U2 U L' U L2 R' U2 L L' R' L' R' L2 R2 U' L R' L L U' R' U2 L2",
    "L R U U2 L2 R U2 L U' L2 R2 U2 L2 L' R' U R2 L2 R' R R' U U' L2 R' R2 U L2 R' U2 L R' U2 L U2 R2 L' R U R L R U L2 R' L2 U2 R U' L U2 R' U L' R2 R L U",
    "L' L' R' R U2 R' U' L' R' U' R' R2 L2 R2 R2 U' L2 R2 U U R' U L' L' L2 U2 R' U' L U' R' R' U R U L2 R L' U L' U R2 R2 L2 L F2",
    "R' L R' L2 U2 L' U2 R2 R L R2 L2 L R U' R2 U' U' R' U R2 U R R' R U2 U L U' R' U' L' R L R U' R' U2 L' U2 L2 U' R' L2",
    "U2 L' R' R2 L2 U L2 R' U2 L2 U R2 U R' L' R2 U' L' R' U2 L2 R2 L U2 R L' R' U R U R' R2 U2 L' R2 L' R' U2 U2 L2 U L2 R L2 R L R' U2 L' R' F2",
    "L2 R2 L' R U' U' L2 L' L2 R2 L' R' R2 U2 L' R2 R U U L R2 U L2 R U2 U' L2 L' U2 R2 R U2 L2 R R L' L U L L2 R' L L2 R2 U2 U R' L2",
    "U' L' U2 L' U' U L R2 L L' R2 L2 U' L2 R' U L' U2 L2 R' R L2 R L R2 L2 R U2 L' R U' L2 R' L' L R2 U2 L' U2 R U L' R R2 L2 R' B'",
    "U' L2 U2 U2 R U L R L2 R' L2 U L2 R U L2 L2 U' U' R2 U2 L R R' U' U' L' L2 R2 L2 R2 U L2 R' L2 U U' U' L2 U R' L U2 L' L R L' R U2 U L U L'",
    "L R2 R2 U' L2 R2 R2 L U2 L' L L R2 L2 R' U U' L2 L' U2 L2 R2 L2 U2 R U2 R U2 R' U' L' R R R' U' L2 R2 U U2 L' R2 U' U' L2 R2 R U' L R U2 F",
    "L R2 L R2 L L2 U2 L U U' R2 U R L' L U2 L' R2 L U' U U U' L R2 R2 U2 L2 U2 R U2 L R' L2 R2 U2 R R' U2 U R' U' L' L2 U R U' L2 R'",
    "L' U' U L L2 R' U' R2 L2 R U' L2 R L' R' R2 U' R' L2 R2 R2 L R2 L' R2 R' U2 R2 U R L' U2 R2 L2 R2 R' L2 R2 U L2 L R U2 L U' L2 R' L' R U2 L2 L' R'",
    "U2 L R L R' U2 L2 U L U2 U L2 R' L' R L2 R2 U R2 R2 L2 R2 L' U' L2 R2 L L R' U2 R' L2 R' U2 L R U L2 U' L2 R L' U2 U2 U L2 R2 U' B",
    "U' L R U' L U2 R2 U2 L2 R2 U2 L2 U L U2 L2 R' L R2 L2 U' L R2 L2 R L U' L R L' U L2 U2 L' R' L L2 R2 U L R U' R L' R R2 U2 U' L2 F'",
    "L' R2 U' L' R2 U L2 R U U L' R' L' U L2 R2 R2 U2 U L' R' U' R L' U L' R2 U2 R' L' L' U' L2 R' U' L2 R U2 L2 U2 R2 U R2 U2 U2 R2 R2 L' R' U R2 U2 L' R D'",
    "L2 R U R' U' U' L U2 L R U2 U2 L U' L' U2 L2 U L' U L R2 U2 L R L' L U' L R L2 L R2 U R' U L2 U R2 R' U2 L2 R L' U2 L2 R U R U2 L U' R' F2",
    "L' U L2 R2 U2 L R' L U L R R R2 R2 L' R2 U' L2 U L R2 L' R' R R U2 L' L2 L R U' L2 L' L2 R L U2 R2 L L U' R2 L'",
    "R U U R U2 R' U2 L R U' R' U' R U2 R' U2 R2 R' U2 L2 R U' R R L2 L2 R2 U2 R' R U U2 R2 L2 U L R' U2 L R' U' U2 L R2 L R L2 L' U' L2 U2",
    "U L2 R' L L2 R2 L R' U2 R' L2 R R' R2 U L R' U2 U2 L U U' L U2 L R2 U' R L2 R R2 U' L2 R U L' R' U2 L U2 R R' U2 R2 R U L R U L' R2 U",
    "U L2 U2 L' R L2 L' R2 U' L' R2 L' R U L' R2 U2 L' L2 R' U2 L' R2 L' U2 R2 U' U' R' U' U L L U2 R U' L R2 U' L2 R2 U' L2 R2 R L' R2 U' U L2 R2 R'",
    "L R2 U2 L2 U' R' L U L2 R L' R2 L' R2 L' R2 U2 L' U' L2 R2 R U2 L2 R2 U2 R2 U R R' U R' L L U L L' R2 L2 R' L' L R U2 R2 U L2 R' B2",
    "R' L' R' R2 U2 R2 U L2 R' R2 U' R2 L' R2 U' R U' R U' U' U L2 R' L' R2 U L U' R2 U' U L R2 U2 R U2 L2 L' R' R U' L' U2 L2 R2 L2 U U' R2 L2 U2",
    "U L2 U2 R U R' U R U L' R2 U L' R' U2 L U2 L L2 R' U2 L R' R2 L R' U2 R2 U' L U2 U' L' U2 U' L R' U' U' R' R' U2 L U U' R2 U2 L2 R R2 L2 U' L U2 R L2",
    "U' L2 R R' U L' U' R2 U2 L2 R L' R' U' L2 U L2 R U2 U2 U' R' U' L2 R' U2 R' U' L2 R R' L' R' U2 R U2 R U L U' R R' U' L2 U2 L' L' R' U L R2 L' R B'",
    "U L R2 R' U' R R' L2 R L2 U' L2 L' R R2 L2 L' U U' L' R R2 U2 L' R U2 L' R' U' R U' L2 R R2 L U R2 L2 R2 L2 R R L2 R' U2 U2 L2",
    "R L L R' L U' R2 L R U2 L R2 U U' L2 U L' R2 R2 U2 L' U2 L' R2 U' R R L' R2 U' R2 U' L R L' L U L R U R2 R' U R2 L' R L' R' L' R2 U L2 R' F2",
    "U L R' U' R' U L2 R' U2 L' U L' R' U2 L2 U' L2 L2 U' L2 R U R2 R2 U2 L2 R' U R' U' L2 U2 L2 U R2 U L R2 U2 L L2 R' L2 R2 U2 L' L U' L D2",
    "R2 U2 L R U2 U' R2 U L2 R' R2 U2 L R' U' R U2 L2 U U2 U R2 U' L R2 R L' U2 L R' U' L R' U' L L' R' L' R L U' U L' R' U' R' U'",
    "L' R' U L2 L2 U' R U R2 R2 U U' L2 U L R' U' L R2 U L' R2 U2 R' U2 U' R' L' R' U2 R U2 R2 U' L' R U' L U U2 U L2 R2 U' L R' R2 L2 R' U2 L2 R' U L2",
    "R' U2 L2 L' R' L U U2 L' R' L2 R2 U2 L2 R L' R' U L' R2 L' U2 L' R2 U2 U R L R' L' R' L2 L R' L' R2 L2 R2 U2 L' R R' U2 L U2 R2 D'",
    "R' R' U' L R R2 U2 L R U2 U' L U2 L' R U' R' U2 L2 R' L2 U2 R' U2 R L U2 L R L R2 U' U U2 L2 U L' L R L' R2 L2 R' L' U' R2 U",
    "U U2 R U2 U2 L L2 R2 L U2 L U' R2 U2 R2 U R' U2 R U' L' R' U' R2 R L' R2 U2 U L L R' U2 L R2 R' L' U L' R2 U2 L R L R' U2 L R' U' U'",
    "R U2 L R2 L U2 L' R2 L2 R' L' R U U' L' R U' U' L R2 R2 U2 L R2 U L' R2 R' L2 L2 R2 R' U' U2 L2 L2 L' U L2 R2 U R U R' L2 R U' U' L' U",
    "U L' R2 U' R2 U R U' R' L2 R L2 R' U R L2 R2 U2 L' R' U2 L R' U' R' U R U' U L R L2 R2 U L' R U2 R2 L U2 L R2 U2 L2 R U' L2 U' R L U2 L' R2 R2 U' B'",
    "U R2 U U' L2 R U2 U L R2 R' U2 L2 R2 U' U2 L2 L' R L' L2 R2 U L2 R' U' L' R L' R2 U L2 R' U2 R U R U2 L R' R R2 U L2 R2 L2 R U R' U' L U' F'",
    "L U U' L' U2 R R U L2 R' L' R' L2 R2 L R2 U2 L2 R U R' U R' U R R R2 U2 L2 R' U2 R U2 U' L R2 U2 U' L R2 U L L U U2 L2 R L' R' U' R2 L2",
    "U R U L' R L R2 R2 L' L R U' R2 L' R R2 U L' U' U' R U' R' U L R2 R2 U2 R U' R U' R2 L' R' U' R2 L U L R' U2 L' U2 R U' L2 R U' R B'",
    "L U' L2 R' R' R2 U' U2 R R2 R2 R U' R R2 U2 U2 L2 R U2 R L' R' U' L R L R2 U L U' L L2 R2 U R2 U L2 R2 U' L2 R U R R' U R' L2 R2 U' L R2 U2 L2 U R'",
    "L U2 L' R U' R U' L2 L' L U2 L2 R2 L' R U L2 R U2 U2 L R L2 U2 L' R' U' R2 L L R2 U' L' U2 L2 U' U R2 L' U R' R' R' U L R' U L' R L' L2 L' U2",
    "U' U2 R2 R U R' U R L2 R L R2 L R L' U U L2 R U2 R' U2 U L2 R2 R' U2 L' U' L2 R' U L' U2 L' R2 U' R2 U2 L L' R' R2 U2 L U L2 R U2 L' L' D2",
    "L2 R L2 U2 U' R U2 R U' L' R L' U R' R' U U R2 U2 L R L' R2 U U' L' R2 R' L R2 L2 R2 L R' U2 L R' L L2 R2 R2 L' R2 L",
    "L R' U2 R U U U L2 R2 L' U R' U' R R' L R U U2 R' U2 L U U' U2 U' R' U2 R L2 R' U' L U' U2 R U' L' L R2 U2 L2 R2 L2 R U U2 F2",
    "L2 L2 R' U R' R' U' L' U' L2 U U U2 L R2 U L' R2 L2 R' L2 L2 R2 L' L' R' L R2 U' L' R U' R2 L R2 U2 L2 R2 U2 L' R' U L U2 L2 R2 B",
    "L L2 R' U2 L' U2 U2 R L R L U' L U R' R2 L2 U' R' U2 L R' R' U' L R2 L2 U2 U' L R U' L2 R R L R U2 U' R U U L' R L' L' U",
    "U2 R U' U2 U' L' U' R U2 L' R2 U' L R2 R' L R L R2 R2 U R2 R R' U R' L L2 L' R R2 U L L2 R L U2 L2 R' R U' L U2"]
